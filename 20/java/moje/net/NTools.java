package moje.net;

import java.io.*;
import java.net.*;

/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */
import java.util.logging.Level;
import java.util.logging.Logger;

/**
 *
 * @author tgrab
 */
public class NTools {

    static URL makeCommand(String method, String args) {
        String server_name = "127.0.0.1";
        String server_port = "9000";
        URL url = null;
        try {
            if (args != null) {
                url = new URL("http://" + server_name + ":" + server_port + "/" + method + "?" + args);
            } else {
                url = new URL("http://" + server_name + ":" + server_port + "/" + method);
            }
        } catch (MalformedURLException ex) {
            ex.printStackTrace();
        }
        return url;

    }

    static String sendAndRead(String method, String args) {
        URL url = makeCommand(method, args);
        //System.out.println(url);
        StringBuffer response = new StringBuffer();
        int ilosc_danych;
        InputStream we = null;


        byte dane[] = new byte[1024];
        try {
            we = url.openStream();
            ilosc_danych = we.read(dane);
            while (ilosc_danych > -1) {
                response.append(new String(dane, 0, ilosc_danych));
                ilosc_danych = we.read(dane);
            }

        } catch (Exception ex) {
            System.out.println("Connection '" + method + "' refused!");
        } finally {
            try {
                we.close();
            } catch (IOException ex) {
                ex.printStackTrace();
            }
        }

        return response.toString();

    }

    static String sendPost(String method, String arg) {
        StringBuffer response = new StringBuffer();
        int ilosc_danych;
        InputStream we = null;
        try {
            URL url = makeCommand(method, null);
            URLConnection uc = url.openConnection();
            uc.setDoOutput(true);
            OutputStreamWriter w = new OutputStreamWriter(new BufferedOutputStream(uc.getOutputStream()));
            w.write(arg);
            w.flush();
            byte dane[] = new byte[1024];
            we = new BufferedInputStream(uc.getInputStream());
            ilosc_danych = we.read(dane);
            while (ilosc_danych > -1) {
                response.append(new String(dane, 0, ilosc_danych));
                ilosc_danych = we.read(dane);
            }
            w.close();
            we.close();

        } catch (IOException ex) {
            Logger.getLogger(NTools.class.getName()).log(Level.SEVERE, null, ex);
        }
        return response.toString();
    }
}
