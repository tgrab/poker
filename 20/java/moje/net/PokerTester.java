package moje.net;

import java.io.BufferedReader;
import java.io.FileReader;

public class PokerTester {

    public static void main(String arg[]) throws Exception {

        BufferedReader plik = new BufferedReader(new FileReader("test.txt"));
        int line=1;
        String s = plik.readLine();
        while (s != null) {
            if (s.indexOf(';')>-1) {s = plik.readLine();line++;continue;}
            
            String kody[] = s.split(" ");
            String resp=null;
            if (kody.length==1) { 
                resp=NTools.sendAndRead(kody[0], null);
            }
            if (kody.length==2) { 
                if (kody[0].equals("action")) {
                 resp=NTools.sendAndRead(kody[0], null);
                 if (!resp.equals(kody[1])) System.out.println("Zla akcja w lini"+line);
                }
                else
                resp=NTools.sendAndRead(kody[0], kody[1]);
            }
            //System.out.println(line);
            System.out.println("in:"+s+" --------->  out:"+resp);
            s = plik.readLine();
            line++;
            Thread.sleep(500);
        }

    }
}