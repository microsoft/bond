package com.microsoft.bond.interop;

import com.microsoft.bond.protocol.FastBinaryReader;
import com.microsoft.bond.protocol.FastBinaryWriter;

import java.io.FileInputStream;
import java.io.FileOutputStream;
import java.io.IOException;
import java.math.BigInteger;
import java.util.Map;
import java.util.TreeMap;
import java.util.TreeSet;

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
            FileInputStream fis = new FileInputStream(fpath);
            OneOfEverything ooe = new OneOfEverything();
            // Skip version for now.
            fis.read(); fis.read(); fis.read(); fis.read();
            ooe.deserialize(new FastBinaryReader<>(fis, (short) 1));

            System.out.println("ooe.is_super_duper: " + ooe.is_super_duper);
            System.out.println(String.format("ooe.b: %d", ooe.b));
            System.out.println(String.format("ooe.s: %d", ooe.s));
            System.out.println(String.format("ooe.i: %d", ooe.i));
            System.out.println(String.format("ooe.l: %d", ooe.l));
            System.out.println(String.format("ooe.ub: %d", ooe.ub));
            System.out.println(String.format("ooe.us: %d", ooe.us));
            System.out.println(String.format("ooe.ui: %d", ooe.ui));
            System.out.println(String.format("ooe.ul: %d", ooe.ul));
            System.out.println(String.format("ooe.f: %f", ooe.f));
            System.out.println(String.format("ooe.d: %f", ooe.d));
            System.out.println(String.format("ooe.bl: %s", ooe.bl ? "true" : "false"));
            System.out.println("ooe.str: " + ooe.str);
            System.out.println("ooe.wstr: " + ooe.wstr);
            System.out.println("ooe.l_b:");
            for (byte b : ooe.l_b) {
                System.out.println(String.format("\t%d", b));
            }
            System.out.println("ooe.v_s:");
            for (short s : ooe.v_s) {
                System.out.println(String.format("\t%d", s));
            }
            System.out.println("ooe.s_ui:");
            for (int ui : ooe.s_ui) {
                System.out.println(String.format("\t%d", ui));
            }
            System.out.println("ooe.m_i_str:");
            for (Map.Entry<Integer, String> e : ooe.m_i_str.entrySet()) {
                System.out.println(String.format("\t%d -> " + e.getValue(), e.getKey()));
            }
            System.out.println(String.format("ooe.e: %d", ooe.e.value));
            System.out.println("ooe.oos.str: " + ooe.oos.str);
            System.out.println("ooe.blorb:");
            for (byte b : ooe.blorb) {
                System.out.println(String.format("\t%d", b));
            }
            System.out.println(String.format("ooe.n_not_null: %d", ooe.n_not_null));
            System.out.println(String.format("ooe.n_null: %s", ooe.n_null));
        } else {
            OneOfEverything ooe = new OneOfEverything();
            ooe.is_super_duper = true;
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
            ooe.s_ui = new TreeSet<>();
            ooe.s_ui.add(3);
            ooe.s_ui.add(33);
            ooe.s_ui.add(333);
            ooe.m_i_str = new TreeMap<>();
            ooe.m_i_str.put(1, "one");
            ooe.m_i_str.put(2, "two");
            ooe.m_i_str.put(3, "three");
            ooe.m_i_str.put(4, "four");
            ooe.m_i_str.put(5, "five");
            ooe.e = SomeEnum.B;
            ooe.oos.str = "one of something, anyway";
            ooe.blorb = new byte[]{1, 2, 3, 4, 5};
            ooe.n_null = null;
            ooe.n_not_null = 5;

            FileOutputStream fos = new FileOutputStream(fpath);
            ooe.marshal(new FastBinaryWriter<>(fos, (short) 1));
        }
    }
}
