--
-- tested: Wireshark-2.2.6 (lua-5.2.4)
--
-- run: wireshark -X lua_script:ndbapi.lua
--

local BLOCK_NO = {
   [0x00f4] = "BACKUP",
   [0x00f5] = "DBTC",
   [0x00f6] = "DBDIH",
   [0x00f7] = "DBLQH",
   [0x00f8] = "DBACC",
   [0x00f9] = "DBTUP",
   [0x00fa] = "DBDICT",
   [0x00fb] = "DBCNTR",
   [0x00fc] = "QMGR",
   [0x00fd] = "NDBFS",
   [0x00fe] = "CMVMI",
   [0x00ff] = "TRIX",
   [0x0100] = "DBUTIL",
   [0x0101] = "SUMA",
   [0x0102] = "DBTUX",
   [0x0103] = "TSMAN",
   [0x0104] = "LGMAN",
   [0x0105] = "PGMAN",
   [0x0106] = "RESTORE",
   [0x0107] = "DBINFO",
   [0x0108] = "DBSPJ",
   [0x0109] = "THRMAN",
   [0x010a] = "TRPMAN",
   --  4f7  = lqhInstanceKey?
   [0x07ff] = "API_PACKED",
   [0x0fa2] = "API_CLUSTERMGR"
}

local BOOL = {
   [0x0000] = "false",
   [0x0001] = "true"
}

local ENDIANNESS = {
   [0x0000] = "little",
   [0x0001] = "big"
}

local GSN = {
   [  1] = "GSN_API_REGCONF",
   [  2] = "GSN_API_REGREF",
   [  3] = "GSN_API_REGREQ",
   [  5] = "GSN_TRANSID_AI",
   [ 10] = "GSN_TCKEYCONF",
   [ 11] = "GSN_TCKEYREF",
   [ 12] = "GSN_TCKEYREQ",
   [ 13] = "GSN_TCROLLBACKCONF",
   [ 14] = "GSN_TCROLLBACKREF",
   [ 15] = "GSN_TCROLLBACKREQ",
   [ 23] = "GSN_GET_TABINFOREF",
   [ 24] = "GSN_GET_TABINFOREQ",
   [ 34] = "GSN_TCRELEASECONF",
   [ 35] = "GSN_TCRELEASEREF",
   [ 36] = "GSN_TCRELEASEREQ",
   [ 37] = "GSN_TCSEIZECONF",
   [ 38] = "GSN_TCSEIZEREF",
   [ 39] = "GSN_TCSEIZEREQ",
   [ 60] = "GSN_ALLOC_NODEID_REQ",
   [ 61] = "GSN_ALLOC_NODEID_CONF",
   [ 62] = "GSN_ALLOC_NODEID_REF",
   [113] = "GSN_API_FAILCONF",
   [114] = "GSN_API_FAILREQ",
   [122] = "GSN_NODE_VERSION_REP",
   [172] = "GSN_COPY_GCICONF",
   [173] = "GSN_COPY_GCIREQ",
   [190] = "GSN_GET_TABINFO_CONF",
   [195] = "GSN_ABORT",
   [197] = "GSN_ABORTED",
   [247] = "GSN_EVENT_REP",
   [275] = "GSN_GCP_COMMIT",
   [276] = "GSN_GCP_NODEFINISH",
   [278] = "GSN_GCP_PREPARE",
   [279] = "GSN_GCP_PREPARECONF",
   [280] = "GSN_GCP_PREPAREREF",
   [314] = "GSN_LQHKEYCONF",
   [315] = "GSN_LQHKEYREF",
   [316] = "GSN_LQHKEYREQ",
   [342] = "GSN_PACKED_SIGNAL",
   [405] = "GSN_TCGETOPSIZECONF",
   [406] = "GSN_TCGETOPSIZEREQ",
   [593] = "GSN_SUB_GCP_COMPLETE_REP",
   [699] = "GSN_SUB_GCP_COMPLETE_ACK"
}

local function word(tvb, bool)
   if bool then return tvb:le_uint() else return tvb:uint() end
end

local proto = Proto("NDBAPI", "MySQL Cluster Transaction and Scanning API")

local fields = proto.fields
fields.gsn                = ProtoField.uint16("ndbapi.gsn",                "gsn",                base.DEC, GSN)
fields.send_block_no      = ProtoField.uint16("ndbapi.send_block_no",      "send_block_no",      base.HEX, BLOCK_NO)
fields.recv_block_no      = ProtoField.uint16("ndbapi.recv_block_no",      "recv_block_no",      base.HEX, BLOCK_NO)
fields.byte_order         = ProtoField.uint8 ("ndbapi.byte_order",         "byte_order",         base.HEX, ENDIANNESS)
fields.checksum_included  = ProtoField.uint8 ("ndbapi.checksum_included",  "checksum_included",  base.HEX, BOOL)
fields.signal_id_included = ProtoField.uint8 ("ndbapi.signal_id_included", "signal_id_included", base.HEX, BOOL)
fields.compressed         = ProtoField.uint8 ("ndbapi.compressed",         "compressed",         base.HEX, BOOL)
fields.message_length     = ProtoField.uint16("ndbapi.message_length",     "message_length",     base.DEC)
fields.fragment_info      = ProtoField.uint8 ("ndbapi.fragment_info",      "fragment_info",      base.HEX)
fields.prio               = ProtoField.uint8 ("ndbapi.prio",               "prio",               base.DEC)
fields.version_id         = ProtoField.uint8 ("ndbapi.version_id",         "version_id",         base.HEX)
fields.trace              = ProtoField.uint8 ("ndbapi.trace",              "trace",              base.HEX)
fields.signal_data_length = ProtoField.uint8 ("ndbapi.signal_data_length", "signal_data_length", base.DEC)
fields.sections_length    = ProtoField.uint8 ("ndbapi.sections_length",    "sections_length",    base.DEC)
fields.signal_id          = ProtoField.uint8 ("ndbapi.signal_id",          "signal_id",          base.HEX)
fields.signal_data        = ProtoField.none  ("ndbapi.signal_data",        "signal_data")

function proto.dissector(tvb, pinfo, tree)

   if tvb(tvb:len() - 1):uint() ~= 0x0a then

      pinfo.cols.protocol = "NDBAPI"

      local b = tvb(0, 1):bitfield(0, 1) == 0
      local w1, w2, w3, n = word(tvb(0, 4), b), word(tvb(4, 4), b), word(tvb(8, 4), b), 12

      local t = tree:add(proto, tvb(), "NDBAPI Protocol Data")
      t:add(fields.gsn,                bit.rshift(bit.band(w2, 0x0000ffff),  0))
      t:add(fields.send_block_no,      bit.rshift(bit.band(w3, 0x0000ffff),  0))
      t:add(fields.recv_block_no,      bit.rshift(bit.band(w3, 0xffff0000), 16))
      t:add(fields.byte_order,         bit.rshift(bit.band(w1, 0x00000001),  8))
      t:add(fields.checksum_included,  bit.rshift(bit.band(w1, 0x00000010),  4))
      t:add(fields.signal_id_included, bit.rshift(bit.band(w1, 0x00000004),  2))
      t:add(fields.compressed,         bit.rshift(bit.band(w1, 0x00000008),  3))
      t:add(fields.message_length,     bit.rshift(bit.band(w1, 0x00ffff00),  8))
      t:add(fields.fragment_info,      bit.bor(
               bit.rshift(bit.band(w1, 0x00000002),  1),
               bit.rshift(bit.band(w1, 0x02000000), 25)
      ))
      t:add(fields.prio,               bit.rshift(bit.band(w1, 0x00000060),  5))
      t:add(fields.version_id,         bit.rshift(bit.band(w2, 0x000f0000), 16))
      t:add(fields.trace,              bit.rshift(bit.band(w2, 0x03f00000), 20))
      t:add(fields.signal_data_length, bit.rshift(bit.band(w1, 0x7c000000), 26))
      t:add(fields.sections_length,    bit.rshift(bit.band(w2, 0x0c000000), 26))

      if bit.rshift(bit.band(w1, 0x00000004), 2) == 1 then -- signal_id_included
         t:add(fields.signal_id, word(tvb(n, 4), b))
         n = n + 4
      end

      local x = bit.rshift(bit.band(w1, 0x7c000000), 26) -- signal_data_length
      if x > 0 then
         local d = t:add(fields.signal_data)
         for i = 0, x - 1 do
            d:add(string.format("[%2d]", i), string.format("0x%08x", word(tvb(n + i * 4, 4), b)))
         end
      end
   end

end

DissectorTable.get("tcp.port"):add("50000-65535", proto) -- TODO
