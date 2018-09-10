
import java.awt.Rectangle;
import java.awt.Robot;
import java.awt.event.InputEvent;
import java.awt.image.BufferedImage;
import java.io.FileInputStream;
import java.util.LinkedList;
import java.util.Properties;
import javax.swing.JOptionPane;

public class Engine implements Runnable {

    Robot robot;
    Rectangle area = new Rectangle(0, 0, 1050, 700);
    Img image = new Img();
    boolean doit = false; //decyduje o dzialaniu glownej petli
    LinkedList<Casino> casinos = new LinkedList<Casino>();
    PokerView view;

    public Engine() {
        try {
            robot = new Robot();
            try {
                Properties defaultProps = new Properties();
                FileInputStream in = new FileInputStream("c:/thx/client/wlasciwosci.properties");
                defaultProps.load(in);
                String cas;
                int xp,xk,yp,yk;
                cas=defaultProps.getProperty("casino1");
                if (cas!=null) {
                    xp=Integer.parseInt(defaultProps.getProperty("xp1"));
                   xk=Integer.parseInt(defaultProps.getProperty("xk1"));
                   yp=Integer.parseInt(defaultProps.getProperty("yp1"));
                   yk=Integer.parseInt(defaultProps.getProperty("yk1"));
                    if (cas.equals("WillHill"))
                        casinos.add(new WillHill(this, new Rectangle(xp, yp,xk,yk)));
                }
                
                
                in.close();
            } catch (Exception wyj) {
                JOptionPane.showMessageDialog(null, "Properties: " + wyj);
            }
          
        } catch (Exception ex) {
            logs("BLAD: " + ex);
        }
    }

    public Engine(PokerView view) {
        this();
        this.view = view;
    }

    public void startEngine() {
        new Thread(this).start();
    }

    public void stopEngine() {
        doit = false;
    }
    
    public boolean isAutoPlay() {
        if (view!=null) return view.isAutoPlay();
        return false;
    }

    public void showStatus() {
        logs(casinos.toString());
    }

    Casino getCasino() {
        return casinos.get(0);
    }

    public static void main(String[] args) throws Exception {
        // TODO code application logic here
        Engine e = new Engine();
        e.showStatus();
        e.getImage().read("1.bmp");
        e.getCasino().synchronize();
         
        
    //e.captureImage();
    // e.image.write("Test");

    }

    public void run() {
        doit = true;
        try {
            captureImage();
            for (Casino c : casinos) {
                c.synchronize();
            }
            for (Casino c : casinos) {
                c.registerClient();
                c.sendAndRead("newgame", null);
            }
            while (doit == true) {
                Thread.sleep(600 - 100 * casinos.size());
                captureImage();
                for (Casino c : casinos) {
                    c.readChat();
                    //karty=searchCards(cardsArea);
                    if (c.isAction()) { //w isAction spr. holecards
                        if (!c.checked_action) {
                            c.Action();
                        }
                        c.checked_action = true;
                    }
                }
            }

        } catch (Exception wyj) {
            JOptionPane.showMessageDialog(null, "run: " + wyj);
        }
    }

    public Img getImage() {
        return image;
    }

    void logs(String m) {
        if (view != null) {
            view.logs(m);
        } else {
            System.out.println(m);
        }
    }

    void click(int x, int y) {
        robot.mouseMove(x, y);
        robot.mousePress(InputEvent.BUTTON1_MASK);
        robot.mouseRelease(InputEvent.BUTTON1_MASK);
    }

    void captureImage() {
        BufferedImage ekran = robot.createScreenCapture(area);
        image.setImage(ekran);
    }
}
