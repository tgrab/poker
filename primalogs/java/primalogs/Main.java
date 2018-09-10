package primalogs;

import java.io.IOException;
import java.sql.Connection;
import java.sql.DriverManager;
import java.sql.ResultSet;
import java.sql.Statement;
import history.*;
import java.io.File;
import java.io.FileInputStream;
import java.io.FileWriter;
import java.io.PrintWriter;
import java.util.Properties;
import java.util.logging.Level;
import java.util.logging.Logger;

public class Main {

    static String dbFile;
    static String logRoot;
    static String mode;

    static boolean dodaj(PrimaLog log, String gametype) {
        //gametype=Omaha
        if (!log.gametype.equals(gametype)) {
            return false;
        }
        String data[] = log.date.split(" ")[0].split("-");
        String tbname = log.tablename.replaceAll(" ", "");
        File f;
        f = new File(logRoot + "/" + data[0] + "/" + data[1] + "/" + data[2] + "/" + tbname);
        //System.out.println(f);
        if (!f.exists()) {
            f.mkdirs();
            new File(f, "xml").mkdir();
        }
        try {
            File f2 = new File(f.toString() + "/xml/" + log.id + ".xml");
            if (f2.exists()) {
                return false;
            }
            PrintWriter wy = new PrintWriter(new FileWriter(f2));
            wy.write(log.xmlSource);
            wy.close();
            f2 = new File(f.toString() + "/" + log.id + ".lsp");
            wy = new PrintWriter(new FileWriter(f2));
            wy.write(log.sexprDesc());
            wy.close();
        } catch (IOException ex) {
            Logger.getLogger(Main.class.getName()).log(Level.SEVERE, null, ex);
        }
        return true;
    }

    static void act() {
        try {

            Connection conn = DriverManager.getConnection("jdbc:sqlite:" + dbFile);
            Statement st = conn.createStatement();
            String q = "SELECT * FROM HandHistory";
            if (mode.equals("last20")) {
                q = "SELECT * FROM HandHistory ORDER BY HandDate DESC LIMIT 0,19";
            }
            ResultSet rs = st.executeQuery(q);
            while (rs.next()) {
                String dump = rs.getString("XMLDump");
                PrimaLog log = new PrimaLog();
                log.parsuj(dump);
                boolean ins = dodaj(log, "Omaha");
                if (ins) {
                    System.out.println(log.id+"@"+log.tablename+" "+log.date);
                }
            }
            conn.close();
        } catch (Exception ex) {
            Logger.getLogger(Main.class.getName()).log(Level.SEVERE, null, ex);
        }
    }

    public static void main(String[] args) {

        Properties props = new Properties();
        try {
            props.load(new FileInputStream("logs.properties"));
        } catch (IOException iOException) {
            System.out.println("Brak pliku wlasciwosci: logs.properties");
            System.exit(0);
        }
        try {
            Class.forName("org.sqlite.JDBC");
        } catch (ClassNotFoundException ex) {
            Logger.getLogger(Main.class.getName()).log(Level.SEVERE, null, ex);
            System.exit(0);
        }

        dbFile = props.getProperty("dbFile", "GameHistory.dat");
        logRoot = props.getProperty("logRoot", "logs");
        mode = props.getProperty("mode", "readAll");

        if (mode.equals("readAll")) {
            act();
        }
        if (mode.equals("last20")) {
            while (true) {
                System.out.println("Connecting: "+new java.util.Date());
                act();
                try {
                    Thread.sleep(30000);
                } catch (InterruptedException ex) {
                    Logger.getLogger(Main.class.getName()).log(Level.SEVERE, null, ex);
                }
            }
        }

    }
}
