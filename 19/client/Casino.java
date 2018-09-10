
import java.awt.Point;
import java.awt.Rectangle;

/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */
import java.io.File;
import java.io.IOException;
import java.io.InputStream;
import java.net.MalformedURLException;
import java.net.URL;
import java.util.ArrayList;
import javax.sound.sampled.AudioFormat;
import javax.sound.sampled.AudioInputStream;
import javax.sound.sampled.AudioSystem;
import javax.sound.sampled.DataLine;
import javax.sound.sampled.SourceDataLine;
import javax.swing.JOptionPane;

/**
 *
 * @author tgrab
 */
public abstract class Casino {

    Engine engine;
    Rectangle area;
    Rectangle chatArea;
    Point synchron;
    String server_name = "127.0.0.1", server_port = "9000";
    boolean sending_remote = true;
    ArrayList<String> sentences = new ArrayList<String>(); //odczytany tekst z czata
    Alphabet alphabet; //alfabet do czata, moga byc jeszcze inne slowniki!
    boolean checked_action = false;
    boolean checked_cards = false;
    byte kind = 3;
    Game game;

    public Casino(Engine engine, Rectangle area) {
        this.engine = engine;
        this.area = area;
        sentences.add("Beginning..."); // readChat chce aby ta lista cos zawierala
        engine.logs("Casino "+this.getClass().getName()+area);
    }

    abstract void Action();

    abstract boolean isAction();

    abstract void synchronize() throws Exception; //ustawia synchronize i chatArea

    abstract String newSentence(String sent);

    abstract void registerClient();

    abstract void doFold();

    abstract void doCall();

    abstract void doCheck();

    abstract void doBet();

    abstract void doPotBet();

    abstract void doPotBet2();

    abstract void doSitOut();
    

    static void playSound(String name) {
        try {
            AudioInputStream ai = AudioSystem.getAudioInputStream(
                    new File("c:\\thx\\client\\sounds\\" + name + ".wav"));
            AudioFormat audioFormat = ai.getFormat();

            // System.out.println(audioFormat);
            DataLine.Info dataLineInfo =
                    new DataLine.Info(
                    SourceDataLine.class,
                    audioFormat);
            SourceDataLine sourceDataLine =
                    (SourceDataLine) AudioSystem.getLine(
                    dataLineInfo);
            sourceDataLine.open(audioFormat);
            sourceDataLine.start();
            int cnt;
            byte tempBuffer[] = new byte[10000];
            while ((cnt = ai.read(tempBuffer, 0, tempBuffer.length)) != -1) {
                if (cnt > 0) {
                    sourceDataLine.write(
                            tempBuffer, 0, cnt);
                }//end if
            }//end while loop
            sourceDataLine.drain();
            sourceDataLine.close();
        } catch (Exception wyj) {
            JOptionPane.showMessageDialog(null, "playSound: " + wyj);
        }
    }

    String soundMessage(String sent) {
        String pol[] = sent.split(" ");
        if (pol.length > 1) {
            playSound(pol[1]);
        }
        return sent;
    }

    URL makeCommand(String method, String args) {
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

    String sendAndRead(String method, String args) {
        URL url = makeCommand(method, args);
        StringBuffer response = new StringBuffer();
        int ilosc_danych;
        InputStream we = null;

        if (sending_remote) {

            byte dane[] = new byte[1024];
            try {
                we = url.openStream();
                ilosc_danych = we.read(dane);
                while (ilosc_danych > -1) {
                    response.append(new String(dane, 0, ilosc_danych));
                    ilosc_danych = we.read(dane);
                }

            } catch (Exception ex) {
                engine.logs("Connection '" + method + "' refused!");
            } finally {
                try {
                    we.close();
                } catch (IOException ex) {
                    ex.printStackTrace();
                }
            }

            return response.toString();
        } else {
            engine.logs(url.toString());
            return "URL: " + url.toString();

        }
    }

    Img getChat() {
        return new Img(engine.getImage().subImage(chatArea));
    }

    /* przy czytaniu czata rozpoznajemy
     * dany znak korzystajac ze specyficznego
     * alfabetu liter
     */
    String recognizeLetter(Img im) {
        return alphabet.teach(im);
    }

    String readSentence(ArrayList<Img> ims) {
        StringBuffer sb = new StringBuffer("");
        for (Img im : ims) {
            sb.append(recognizeLetter(im));
        }
        return sb.toString().replaceAll("_", "");
    }

    String[] readSentences() {
        Img chat = getChat();
        String sents[];
        ArrayList<Img> rows = chat.getRows();
        sents = new String[rows.size()];
        for (int i = 0; i < rows.size(); i++) {
            sents[i] = readSentence(rows.get(i).getCols());
        }
        return sents;
    }

    int checkSentences(String sents[]) {
        int i,
                ilosc = sentences.size();
        String last = sentences.get(ilosc - 1);
        for (i = sents.length - 1; i >= 0; i--) {
            if (last.equals(sents[i])) {
                break;
            }
        }
        return i;
    }

    String readChat() {
        String sents[] = readSentences();
        String lastSent = null;
        int nowe = checkSentences(sents);
        String sent;
        for (int i = nowe + 1; i < sents.length; i++) {
            sent = sents[i];
            sentences.add(sent);
            engine.logs(sent);
            checked_action = false;
            lastSent = newSentence(sent);
        }
        return lastSent;
    }
}
