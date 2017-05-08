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
            ooe.ub = (byte) 128;
            ooe.us = (short) 32768;
            ooe.ui = (int) 2147483648L;
            ooe.ul = BigInteger.valueOf(Long.MAX_VALUE).add(BigInteger.ONE).longValue();
            ooe.f = .5f;
            ooe.d = .5;
            ooe.bl = true;
            ooe.str = "three";
            ooe.wstr = "threeeee";
            ooe.l_b.add((byte) 3);
            ooe.l_b.add((byte) 33);
            ooe.v_s.add((short) 3);
            ooe.v_s.add((short) 33);
            ooe.s_ui.add(3);
            ooe.s_ui.add(33);
            ooe.s_ui.add(333);
            ooe.m_i_str.put(1, "one");
            ooe.m_i_str.put(2, "two");
            ooe.m_i_str.put(3, "three");
            ooe.m_i_str.put(4, "four");
            ooe.m_i_str.put(5, "five");
            ooe.e = SomeEnum.B;
            ooe.oos.str = "one of something, anyway";

            FileOutputStream fos = new FileOutputStream(fpath);
            ooe.marshal(new FastBinaryWriter<>(fos, (short) 1));
        }
    }
}
