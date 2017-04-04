package com.microsoft.bond.interop;

import com.microsoft.bond.protocol.FastBinaryWriter;

import java.io.FileOutputStream;
import java.io.IOException;
import java.math.BigInteger;

public class Main {
    public static void main(String[] args) throws IOException {
        if (args.length != 2) {
            System.out.println("Usage: java -jar interop.jar read|write file");
            System.exit(255);
        }

        boolean read_mode = false;
        switch (args[0]) {
            case "read":
                read_mode = true;
                break;
            case "write":
                read_mode = false;
                break;
            default:
                System.out.println("Unknown mode: " + args[0]);
        }

        String fpath = args[1];

        if (read_mode) {
            throw new RuntimeException("Not implemented!");
        } else {
            OneOfEverything ooe = new OneOfEverything();
            ooe.b = 3;
            ooe.s = 333;
            ooe.i = 333333;
            ooe.l = 33333333333L;
            ooe.ub = 128;
            ooe.us = 32768;
            ooe.ui = 2147483648L;
            ooe.ul = BigInteger.valueOf(Long.MAX_VALUE).add(BigInteger.ONE);
            ooe.f = .5f;
            ooe.d = .5;
            ooe.bl = true;
            ooe.str = "three\0";
            ooe.wstr = "threeeee";
            ooe.l_b.add((byte) 3);
            ooe.l_b.add((byte) 33);
            ooe.v_f.add(.5f);
            ooe.v_f.add(.75f);
            ooe.s_ui.add((long) 3);
            ooe.s_ui.add((long) 33);
            ooe.s_ui.add((long) 333);
            ooe.m_i_str.put(1, "one");
            ooe.m_i_str.put(2, "two");
            ooe.m_i_str.put(3, "three");

            FileOutputStream fos = new FileOutputStream(fpath);
            ooe.marshal(new FastBinaryWriter<>(fos, (short) 1));
        }
    }
}
