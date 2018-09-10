/*
 * Zarzadza skanowaniem ekranu
 * Beda zainstalowane rozne casina
 */
package engine;

import com.sun.speech.freetts.Voice;
import com.sun.speech.freetts.VoiceManager;
import java.awt.AWTException;
import java.awt.Point;
import java.awt.Rectangle;
import java.awt.Robot;
import java.awt.event.InputEvent;
import java.awt.image.BufferedImage;
import java.io.FileInputStream;
import java.io.FileWriter;
import java.io.IOException;
import java.io.PrintWriter;
import java.util.Properties;
import java.util.logging.Level;
import java.util.logging.Logger;
import javax.swing.JOptionPane;
import javax.sound.sampled.AudioFormat;
import javax.sound.sampled.AudioInputStream;
import javax.sound.sampled.AudioSystem;
import javax.sound.sampled.DataLine;
import javax.sound.sampled.SourceDataLine;

class EmptyLogger implements Logs {

    public void messg(int level, String s) {
    }
}

class ConsoleLogger implements Logs {

    int logsLevel = 0;

    public void messg(int level, String s) {
        if (level >= logsLevel) {
            System.out.println(s);
        }
    }
}


class FileLogger implements Logs {

    int logsLevel = 0;
    PrintWriter wy;
    public FileLogger() {
        try {
           wy = new PrintWriter(new FileWriter("Screen.log"));
           wy.println(new java.util.Date().toString());
           // wy = new PrintWriter(new FileWriter(System.currentTimeMillis()+".log"));
        } catch (IOException ex) {
            Logger.getLogger(FileLogger.class.getName()).log(Level.SEVERE, null, ex);
        }
    }


    public void messg(int level, String s) {
        if (level >= logsLevel) {
            wy.println(s);
            wy.flush();
        }
    }
}
public class Screen implements Runnable {

    Robot robot;
    public static Logs logs=new FileLogger();
    Rectangle area;
    public static Img image = new Img();
    boolean doit = false; //decyduje o dzialaniu glownej petli
    Table tables[];
    Properties props=new Properties();

    public forms.Main mainForm;
    

    public Screen() {
        System.setProperty("freetts.voices", "com.sun.speech.freetts.en.us.cmu_us_kal.KevinVoiceDirectory");
        try {
            logs.messg(1,"Loading wlasciwosci ...");
            props.load(new FileInputStream("thx.props"));
        } catch (IOException ex) {
            System.out.println(ex);
        }
        String mode=props.getProperty("mode", "pl1");
        //default mode
        if (mode.equals("pl1")) {
        area = new Rectangle(0, 0, 1050, 650);
        tables = new Table[1];
        tables[0] = new TablePL(this, area, props);
        }

        try {
            robot = new Robot();
        } catch (AWTException ex) {
            Logger.getLogger(Screen.class.getName()).log(Level.SEVERE, null, ex);
        }
    }

    public void sayIt(String text) {      
        Voice voice=VoiceManager.getInstance().getVoice("kevin");
        voice.allocate();
        voice.speak(text);
        voice.deallocate();
    }

    public static void playSound(String name) {
        try {
            AudioInputStream ai = AudioSystem.getAudioInputStream(
                    Screen.class.getResourceAsStream("/sounds/" + name + ".wav"));

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

    void click(int x, int y) {
        Point oldPos=java.awt.MouseInfo.getPointerInfo().getLocation();
        robot.mouseMove(x, y);
        robot.mousePress(InputEvent.BUTTON1_MASK);
        robot.mouseRelease(InputEvent.BUTTON1_MASK);
        robot.mouseMove(oldPos.x, oldPos.y);
    }

    public void captureImage() {
        BufferedImage ekran = robot.createScreenCapture(area);
        image.setImage(ekran);
    }

    public Table getFirstTable() {
        return tables[0];
    }

    public void startEngine() {
        new Thread(this).start();
    }

    public void stopEngine() {
        doit = false;
    }

    public void run() {
        doit = true;
        try {
            captureImage();
            for (Table c : tables) {
                c.synchronize();
            }

            while (doit == true) {
                Thread.sleep(500);
                captureImage();
                for (Table c : tables) {
                    c.readChat();
                    //karty=searchCards(cardsArea);
                    if (c.isAction()) { //w isAction spr. holecards
                        if (!c.checked_action) {
                            c.outAction();
                        }
                        c.checked_action = true;
                    }
                }
            }

        } catch (Exception wyj) {
           // JOptionPane.showMessageDialog(null, "run: " + wyj);
            logs.messg(10, "Screen.run exception:"+wyj);
            sayIt("Exception in screen. Program stopped");
            if (mainForm!=null) {
                mainForm.bStart.setEnabled(true);
                mainForm.bStop.setEnabled(false);
            }
        }
    }
    public static void main(String arg[]) {
        playSound("beep");
//        Screen sc=new Screen();
//        Img imTest=new Img();
//        imTest.read("Ekran.bmp");
//        sc.image.setImage(imTest.image);
//        //sc.image.show();
//        Table t1=sc.getFirstTable();
//        System.out.println(t1.synchron);
//        t1.synchronize();
//        System.out.println(t1.synchron);
//        t1.getChat().im2BW();
//        t1.getChat().write("chat");

    }
}
