ndbapi_proto = Proto("NDBAPI", "MySQL Cluster Transaction and Scanning API")

Bool = {
    [0x0000] = "Off",
    [0x0001] = "On"
}

Endianness = {
    [0x0000] = "little",
    [0x0001] = "big"
}

f = ndbapi_proto.fields
f.gsn                = ProtoField.uint16("ndbapi.gsn",                "gsn",                base.DEC)
f.send_block_no      = ProtoField.uint16("ndbapi.send_block_no",      "send_block_no",      base.DEC)
f.recv_block_no      = ProtoField.uint16("ndbapi.recv_block_no",      "recv_block_no",      base.DEC)
f.byte_order         = ProtoField.uint8 ("ndbapi.byte_order",         "byte_order",         base.HEX, Endianness)
f.checksum_included  = ProtoField.uint8 ("ndbapi.checksum_included",  "checksum_included",  base.HEX, Bool)
f.signal_id_included = ProtoField.uint8 ("ndbapi.signal_id_included", "signal_id_included", base.HEX, Bool)
f.compressed         = ProtoField.uint16("ndbapi.compressed",         "compressed",         base.HEX, Bool)
f.message_length     = ProtoField.uint16("ndbapi.message_length",     "message_length",     base.DEC)
f.fragment_info      = ProtoField.uint8 ("ndbapi.fragment_info",      "fragment_info",      base.HEX)
f.prio               = ProtoField.uint8 ("ndbapi.prio",               "prio",               base.DEC)
f.version_id         = ProtoField.uint8 ("ndbapi.version_id",         "version_id",         base.HEX)
f.trace              = ProtoField.uint16("ndbapi.trace",              "trace",              base.HEX)
f.signal_data_length = ProtoField.uint16("ndbapi.signal_data_length", "signal_data_length", base.DEC)
f.sections_length    = ProtoField.uint16("ndbapi.sections_length",    "sections_length",    base.DEC)

function ndbapi_proto.dissector(tvb, pinfo, tree)

    pinfo.cols.protocol = "ndbapi"

    local w1, w2, w3
    if tvb(0, 1):bitfield(0, 1) == 0 then
      w1, w2, w3 = tvb(0, 4):le_uint(), tvb(4, 4):le_uint(), tvb(8, 4):le_uint()
    else
      w1, w2, w3 = tvb(0, 4):uint(), tvb(4, 4):uint(), tvb(8, 4):uint()
    end

    local t = tree:add(ndbapi_proto, tvb(), "ndbapi protocol data")
    t:add(f.gsn,                bit.rshift(bit.band(w2, 0x0000ffff),  0))
    t:add(f.send_block_no,      bit.rshift(bit.band(w3, 0x0000ffff),  0))
    t:add(f.recv_block_no,      bit.rshift(bit.band(w3, 0xffff0000), 16))
    t:add(f.byte_order,         bit.rshift(bit.band(w1, 0x00000001),  8))
    t:add(f.checksum_included,  bit.rshift(bit.band(w1, 0x00000010),  4))
    t:add(f.signal_id_included, bit.rshift(bit.band(w1, 0x00000004),  2))
    t:add(f.compressed,         bit.rshift(bit.band(w1, 0x00000008),  3))
    t:add(f.message_length,     bit.rshift(bit.band(w1, 0x00ffff00),  8))
    t:add(f.fragment_info,      bit.bor(
                                bit.rshift(bit.band(w1, 0x00000002),  1),
                                bit.rshift(bit.band(w1, 0x02000000), 25)
                                ))
    t:add(f.prio,               bit.rshift(bit.band(w1, 0x00000060),  5))
    t:add(f.version_id,         bit.rshift(bit.band(w2, 0x000f0000), 16))
    t:add(f.trace,              bit.rshift(bit.band(w2, 0x03f00000), 20))
    t:add(f.signal_data_length, bit.rshift(bit.band(w1, 0x7c000000), 26))
    t:add(f.sections_length,    bit.rshift(bit.band(w2, 0x0c000000), 26))

end

DissectorTable.get("tcp.port"):add(50000, ndbapi_proto) -- TODO
