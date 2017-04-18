ndbapi_proto = Proto("NDBAPI", "MySQL Cluster Transaction and Scanning API")

function ndbapi_proto.dissector(tvb, pinfo, tree)

    pinfo.cols.protocol = "ndbapi"

    local w1, w2, w3
    if tvb(0, 1):bitfield(0, 1) == 0 then
      w1, w2, w3 = tvb(0, 4):le_uint(), tvb(4, 4):le_uint(), tvb(8, 4):le_uint()
    else
      w1, w2, w3 = tvb(0, 4):uint(), tvb(4, 4):uint(), tvb(8, 4):uint()
    end

    local t = tree:add(ndbapi_proto, tvb(), "ndbapi protocol data")
    t:add(bit.rshift(bit.band(w2, 0x0000ffff),  0), " : gsn")
    t:add(bit.rshift(bit.band(w3, 0x0000ffff),  0), " : send_block_no")
    t:add(bit.rshift(bit.band(w3, 0xffff0000), 16), " : recv_block_no")
    t:add(bit.rshift(bit.band(w1, 0x00000001),  8), " : byte_order")
    t:add(bit.rshift(bit.band(w1, 0x00000010),  4), " : checksum_included")
    t:add(bit.rshift(bit.band(w1, 0x00000004),  2), " : signal_id_included")
    t:add(bit.rshift(bit.band(w1, 0x00000008),  3), " : compressed")
    t:add(bit.rshift(bit.band(w1, 0x00ffff00),  8), " : message_length")
    t:add(bit.rshift(bit.band(w1, 0x00000002),  1), " : fragment_info_1")
    t:add(bit.rshift(bit.band(w1, 0x02000000), 25), " : fragment_info_2")
    t:add(bit.rshift(bit.band(w1, 0x00000060),  5), " : prio")
    t:add(bit.rshift(bit.band(w2, 0x000f0000), 16), " : version_id")
    t:add(bit.rshift(bit.band(w2, 0x03f00000), 20), " : trace")
    t:add(bit.rshift(bit.band(w1, 0x7c000000), 26), " : signal_data_length")
    t:add(bit.rshift(bit.band(w2, 0x0c000000), 26), " : sections_length")

end

DissectorTable.get("tcp.port"):add(50000, ndbapi_proto) -- TODO
