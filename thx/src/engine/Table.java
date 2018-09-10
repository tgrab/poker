package engine;

import java.awt.Color;
import java.awt.Dimension;
import java.awt.Graphics;
import java.awt.Graphics2D;
import java.awt.Point;
import java.awt.Rectangle;
import java.awt.image.BufferedImage;
import java.io.*;
import java.net.Socket;
import java.util.ArrayList;
import java.util.Properties;
import java.util.logging.Level;
import java.util.logging.Logger;
import javax.imageio.ImageIO;
import javax.swing.JFrame;
import javax.swing.JOptionPane;
import javax.swing.JPanel;
import org.armedbear.lisp.*;

/**
 *
 * @author tgrab
 */
public abstract class Table extends JPanel {

    static Screen screen;
    Properties props;
    Interpreter lisp;
    Point synchron;
    Rectangle area;
    Rectangle chatArea;
    ArrayList<String> sentences = new ArrayList<String>(); //odczytany tekst z czata
    public Alphabet alphabet; //alfabet do czata, moga byc jeszcze inne slowniki!
    boolean checked_action = false;
    public Game game = new Game("007");
    boolean withLisp = true;
    boolean localLisp = true;
    String remoteLispHost = "127.0.0.1";
    int remoteLispPort = 9000;
    boolean withRepaint = true;
    public boolean withAutoPlay = true;
    String msgMemory = "";
    String msgOut = "";
    String msgIn = "";
    String msgCards = "";
    boolean agentActive=true;
    String msgTableCards = "";
    public int opoznienieAkcji=0;
    public int numOfPlayers0=10;
    public int numOfPlayers1=10;

    public Table(Screen s,Rectangle area,Properties p) {
        screen=s;
        this.area=area;
        props=p;
        writeLog(0, "Installing Alphabet ...");
        installAlphabet();
        writeLog(0, "Installing Alphabet:done");
        sentences.add("Beginning..."); // readChat chce aby ta lista cos zawierala
        localLisp=props.getProperty("localLisp", "true").equals("true");
        if (withLisp) {
            if (localLisp) {
                writeLog(0, "Installing Lisp Systems ...");
                lisp = Interpreter.createInstance();
                evalLisp("(load \"sys\")");
                evalLisp("(sys-load)");
            }

            evalLisp("(setq *game* (new-game))");
            writeLog(0, "Installing Lisp Systems:done");
        }
    }

    @Override
    public Dimension getPreferredSize() {
        return new Dimension(300, 150);
    }

    @Override
    protected void paintComponent(Graphics g0) {
        super.paintComponent(g0);
        //Graphics2D g=(Graphics2D)g0;
        g0.drawString("=> "+msgOut, 5, 10);
        if (msgIn.contains("ACTION")) g0.setColor(Color.red);
        g0.drawString("<= "+msgIn, 5, 20);
        g0.setColor(Color.black);
        g0.drawString(msgCards, 5, 30);
        g0.drawString(msgTableCards, 5, 40);
    }


    public Interpreter getLisp() {
        return lisp;
    }

    public Game getGame() {
        return game;
    }

    protected void writeLog(int l, String s) {
        if (screen == null) {
            System.out.println(" -- " + s);
        } else {
            Screen.logs.messg(l, s);
        }
    }

    public abstract void synchronize();

    abstract void installChatArea();

    abstract void installAlphabet();

    abstract boolean isAction();

    abstract void actionFold();

    abstract void actionCall();

    abstract void actionBet();

    abstract void actionPotBet1();

    abstract void actionPotBet2();

    abstract void actionSitOut();

    abstract void newSentence(String s);
    abstract String agentName();

    public Img getChat() {
        if (chatArea == null) {
            installChatArea();
        }
        return new Img(Screen.image.subImage(chatArea));
    }

    public String evalLisp(String sexpr) {
        if (!sexpr.startsWith("(ga"))msgOut=sexpr;
        if (!withLisp) {
            writeLog(0, "NOT sending " + sexpr);
            return "";
        }
        writeLog(1, "=> " + sexpr);
        if (!localLisp) {
            StringBuilder sb = new StringBuilder();
            try {
                Socket g = new Socket(remoteLispHost, remoteLispPort);
                OutputStream wy = g.getOutputStream();
                InputStream we = g.getInputStream();
                byte buffer[] = new byte[1024];
                int ile = 0;
                wy.write(sexpr.getBytes());
                wy.write("\n".getBytes());
                wy.flush();
                
                ile = we.read(buffer);
                while (ile != -1) {
                    sb.append(new String(buffer, 0, ile));
                    ile = we.read(buffer);
                }
                wy.close();
                we.close();
                g.close();
            } catch (Exception ex) {
                msgOut=msgOut+" "+ex;
                writeLog(1, "Connection problem" + ex);
            }
            return sb.toString();
        }
        try {
            LispObject res = lisp.eval(sexpr);
            String res2 = res.writeToString();
            if (res2.startsWith("\"")) {
                res2 = res2.substring(1, res2.length() - 1);
            }
            return res2;
        } catch (ConditionThrowable ex) {
            Logger.getLogger(Table.class.getName()).log(Level.SEVERE, null, ex);
        }
        return "";
    }
    // percepts:

    public void newGame(String id) {
        numOfPlayers0=numOfPlayers1;
        numOfPlayers1=game.freeseat;
        if (numOfPlayers0<=2 && numOfPlayers1<=2) {
            screen.sayIt("Three players left. Sitting out.");
            actionSitOut();
        }
        game = new Game(id);
        evalLisp("(setq *game* (new-game :id " + id + " :prev-game *game* :save-log t))");
        msgMemory = "";
        msgIn = "";
        msgCards="";
        msgTableCards="";
        agentActive=true;
        if (withRepaint) {
            repaint();
        }
    }

    public void smallBlind(String player, String amt) {
        game.smallBlind(player, Float.parseFloat(amt));
        msgIn = evalLisp("(small-blind  \"" + player + "\" " + amt + " *game*)");
        if (withRepaint) {
            repaint();
        }
    }

    public void bigBlind(String player, String amt) {
        game.bigBlind(player, Float.parseFloat(amt));
        msgIn = evalLisp("(big-blind \"" + player + "\" " + amt + " *game*)");
        if (withRepaint) {
            repaint();
        }
    }

    public void call(String player, String amt) {
        game.call(player, Float.parseFloat(amt));
        msgIn = evalLisp("(call  \"" + player + "\" " + amt + " *game*)");
        if (withRepaint) {
            repaint();
        }
    }

    public void bet(String player, String amt) {
        game.bet(player, Float.parseFloat(amt));
        msgIn = evalLisp("(bet  \"" + player + "\" " + amt + " *game*)");
        if (withRepaint) {
            repaint();
        }
    }

    public void raise(String player, String amt) {
        game.raise(player, Float.parseFloat(amt));
        msgIn = evalLisp("(raise  \"" + player + "\" " + amt + " *game*)");
        if (withRepaint) {
            repaint();
        }
    }

    public void allIn(String player, String amt) {
        game.allIn(player, Float.parseFloat(amt));
        msgIn = evalLisp("(allin  \"" + player + "\" " + amt + " *game*)");
        if (withRepaint) {
            repaint();
        }
    }

    public void allIn(String player) {
        game.allIn(player);
        msgIn = evalLisp("(allin  \"" + player + "\" *game*)");
        if (withRepaint) {
            repaint();
        }
    }

    public void fold(String player) {
        game.fold(player);
        msgIn = evalLisp("(fold  \"" + player + "\" *game*)");
        if (withRepaint) {
            repaint();
        }
    }

    public void holeCards(int h1, int h2, int h3, int h4) {
        game.holecards(h1, h2, h3, h4);
        msgCards=evalLisp(String.format("(holecards *game* %d %d %d %d)", h1, h2, h3, h4));
        if (withRepaint) {
//            msgMemory = evalLisp("(game-memory)");
            repaint();
        }
    }

    void describeRound() {
        if (!agentActive) return;
        if (msgMemory.indexOf(" RANK-NUTS ")>-1) {
            screen.sayIt("Attention, Rank nuts");
            return;
        }
        if (msgMemory.indexOf(" RANK-FULLHOUSE ")>-1) {
            screen.sayIt("Attention, Rank full house");
            return;
        }
        if (msgMemory.indexOf(" RANK-SET ")>-1) {
            screen.sayIt("Attention, Rank set");
            return;
        }
        if (msgMemory.indexOf(" RANK-COLOR ")>-1) {
            screen.sayIt("Attention, Rank color");
            return;
        }
        if (msgMemory.indexOf(" RANK-STR8 ")>-1) {
            screen.sayIt("Attention, Rank straight");
            return;
        }
        if (msgMemory.indexOf(" RANK-TRIPS ")>-1) screen.sayIt("Attention, Rank trips");
    }

    public void flop(int h1, int h2, int h3) {
        game.flop(h1, h2, h3);
        msgTableCards=evalLisp(String.format("(round-flop %d %d %d *game*)", h1, h2, h3));
      //  msgAction = evalLisp("(awhen (rank *game*) (render-rank it))");
        if (agentActive) msgMemory = evalLisp("(game-memory)");
        describeRound();
        if (withRepaint) {
            repaint();
        }
    }

    public void turn(int h1) {
        game.turn(h1);
        msgTableCards=evalLisp(String.format("(round-turn %d *game*)", h1));
//        msgAction = evalLisp("(awhen (rank *game*) (render-rank it))");
         if (agentActive) msgMemory = evalLisp("(game-memory)");
        describeRound();
        if (withRepaint) {
            repaint();
        }
    }

    public void river(int h1) {
        game.river(h1);
        msgTableCards=evalLisp(String.format("(round-river %d *game*)", h1));
//        msgAction = evalLisp("(awhen (rank *game*) (render-rank it))");
        msgMemory = evalLisp("(game-memory)");
         if (agentActive) describeRound();
        if (withRepaint) {
            repaint();
        }
    }

    public void outAction() {
        String a = evalLisp("(action)");
        if (withRepaint) {
            msgIn = "ACTION = "+a;
           // msgDecision = evalLisp("(show-action-rules-result)");
            msgMemory = evalLisp("(game-memory)");
            repaint();
        }
        if (!withAutoPlay) {
            return;
        }

        

        int opoznienie=0;
        int losCzas=2000;
        if (a.equals("fold")) {
            if (game.playerCommited(agentName())) {
                screen.sayIt("Fold commited");
                opoznienie=opoznienieAkcji;
                losCzas=5000;
            }
        }
        if (a.equals("call")) {
            if (msgMemory.indexOf(" STAKE-BIG ")>-1) {
                 screen.sayIt("Call big stake");
                 opoznienie=opoznienieAkcji;
                 losCzas=5000;
            }

        }
        if (a.equals("fuzzy-call")) {
            if (msgMemory.indexOf(" STAKE-BIG ")>-1)
            {   screen.sayIt("Fuzzy call");
                opoznienie=opoznienieAkcji;
                losCzas=5000;
            }
        }
        if (a.equals("bet")) {

        }
        if (a.equals("potbet1")) {
            screen.sayIt("Bet pot");
            opoznienie=opoznienieAkcji;
            losCzas=5000;
        }
        if (a.equals("potbet2")) {
            screen.sayIt("Bet");
        }
        
        try {
            Thread.sleep(opoznienie+Math.round(losCzas * Math.random()));
        } catch (InterruptedException ex) {
            Logger.getLogger(Table.class.getName()).log(Level.SEVERE, null, ex);
        }
        if (!withAutoPlay) {
            return;
        }
        if (a.equals("fold")) {
            actionFold();
            agentActive=false;
        }
        if (a.equals("call")) {
            actionCall();
        }
        if (a.equals("fuzzy-call")) {
            actionCall();
        }
        if (a.equals("bet")) {
            actionBet();
        }
        if (a.equals("potbet1")) {
            actionPotBet1();
        }
        if (a.equals("potbet2")) {
            actionPotBet2();
        }

    }

    /* przy czytaniu czata rozpoznajemy
     * dany znak korzystajac ze specyficznego
     * alfabetu liter
     */
    String recognizeLetter(Img im) {
        return getAlphabet().teach(im);
    }

    String readSentence(ArrayList<Img> ims) {
        StringBuffer sb = new StringBuffer("");
        for (Img im : ims) {
            if (im.height == 1 && im.width == 1) {
                sb.append('.');
                continue;
            }
            if (im.height >= 5) {
                sb.append(recognizeLetter(im));
            }
        }
        return sb.toString().replaceAll("_", "");
    }

    public String[] readSentences() {
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
        int i, ilosc = sentences.size();
        String last = sentences.get(ilosc - 1);
        for (i = sents.length - 1; i >= 0; i--) {
            if (last.equals(sents[i])) {
                break;
            }
        }
        return i;
    }

    void readChat() {
        String sents[] = readSentences();
        int nowe = checkSentences(sents);
        String sent;
        for (int i = nowe + 1; i < sents.length; i++) {
            sent = sents[i];
            sentences.add(sent);
            writeLog(1,"chat: "+sent);
            checked_action = false;
            try {
                newSentence(sent);
            } catch (Exception e) {
                //JOptionPane.showMessageDialog(null, "newSentence exception: " + sent);
                screen.sayIt("Exception in new sentence.");
                writeLog(1, "SENTENCE ["+ sent+"] generated"+ e.toString());
            }
        }

    }

    /**
     * @return the alphabet
     */
    public //odczytany tekst z czata
            Alphabet getAlphabet() {
        return alphabet;
    }
}
