
import java.awt.Point;
import java.awt.Rectangle;

/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */
import java.util.ArrayList;
import java.util.List;
import java.util.logging.Level;
import java.util.logging.Logger;

public class WillHill extends Casino {

    Img[] cards;
    Rectangle agent;
    Img agentAction = new Img("c:/thx/client/alfabet/willhillcards/GrabolInv.bmp");

    public WillHill(Engine engine, Rectangle area) {
        super(engine, area);
        alphabet = new Alphabet("c:/thx/client/alfabet/willhill");
        cards = new Img[52];
        cards[0] = new Img("c:/thx/client/alfabet/willhillcards/As.bmp");
        cards[1] = new Img("c:/thx/client/alfabet/willhillcards/Ks.bmp");
        cards[2] = new Img("c:/thx/client/alfabet/willhillcards/Qs.bmp");
        cards[3] = new Img("c:/thx/client/alfabet/willhillcards/Js.bmp");
        cards[4] = new Img("c:/thx/client/alfabet/willhillcards/Ts.bmp");
        cards[5] = new Img("c:/thx/client/alfabet/willhillcards/9s.bmp");
        cards[6] = new Img("c:/thx/client/alfabet/willhillcards/8s.bmp");
        cards[7] = new Img("c:/thx/client/alfabet/willhillcards/7s.bmp");
        cards[8] = new Img("c:/thx/client/alfabet/willhillcards/6s.bmp");
        cards[9] = new Img("c:/thx/client/alfabet/willhillcards/5s.bmp");
        cards[10] = new Img("c:/thx/client/alfabet/willhillcards/4s.bmp");
        cards[11] = new Img("c:/thx/client/alfabet/willhillcards/3s.bmp");
        cards[12] = new Img("c:/thx/client/alfabet/willhillcards/2s.bmp");

        cards[13] = new Img("c:/thx/client/alfabet/willhillcards/Ah.bmp");
        cards[14] = new Img("c:/thx/client/alfabet/willhillcards/Kh.bmp");
        cards[15] = new Img("c:/thx/client/alfabet/willhillcards/Qh.bmp");
        cards[16] = new Img("c:/thx/client/alfabet/willhillcards/Jh.bmp");
        cards[17] = new Img("c:/thx/client/alfabet/willhillcards/Th.bmp");
        cards[18] = new Img("c:/thx/client/alfabet/willhillcards/9h.bmp");
        cards[19] = new Img("c:/thx/client/alfabet/willhillcards/8h.bmp");
        cards[20] = new Img("c:/thx/client/alfabet/willhillcards/7h.bmp");
        cards[21] = new Img("c:/thx/client/alfabet/willhillcards/6h.bmp");
        cards[22] = new Img("c:/thx/client/alfabet/willhillcards/5h.bmp");
        cards[23] = new Img("c:/thx/client/alfabet/willhillcards/4h.bmp");
        cards[24] = new Img("c:/thx/client/alfabet/willhillcards/3h.bmp");
        cards[25] = new Img("c:/thx/client/alfabet/willhillcards/2h.bmp");

        cards[26] = new Img("c:/thx/client/alfabet/willhillcards/Ad.bmp");
        cards[27] = new Img("c:/thx/client/alfabet/willhillcards/Kd.bmp");
        cards[28] = new Img("c:/thx/client/alfabet/willhillcards/Qd.bmp");
        cards[29] = new Img("c:/thx/client/alfabet/willhillcards/Jd.bmp");
        cards[30] = new Img("c:/thx/client/alfabet/willhillcards/Td.bmp");
        cards[31] = new Img("c:/thx/client/alfabet/willhillcards/9d.bmp");
        cards[32] = new Img("c:/thx/client/alfabet/willhillcards/8d.bmp");
        cards[33] = new Img("c:/thx/client/alfabet/willhillcards/7d.bmp");
        cards[34] = new Img("c:/thx/client/alfabet/willhillcards/6d.bmp");
        cards[35] = new Img("c:/thx/client/alfabet/willhillcards/5d.bmp");
        cards[36] = new Img("c:/thx/client/alfabet/willhillcards/4d.bmp");
        cards[37] = new Img("c:/thx/client/alfabet/willhillcards/3d.bmp");
        cards[38] = new Img("c:/thx/client/alfabet/willhillcards/2d.bmp");

        cards[39] = new Img("c:/thx/client/alfabet/willhillcards/Ac.bmp");
        cards[40] = new Img("c:/thx/client/alfabet/willhillcards/Kc.bmp");
        cards[41] = new Img("c:/thx/client/alfabet/willhillcards/Qc.bmp");
        cards[42] = new Img("c:/thx/client/alfabet/willhillcards/Jc.bmp");
        cards[43] = new Img("c:/thx/client/alfabet/willhillcards/Tc.bmp");
        cards[44] = new Img("c:/thx/client/alfabet/willhillcards/9c.bmp");
        cards[45] = new Img("c:/thx/client/alfabet/willhillcards/8c.bmp");
        cards[46] = new Img("c:/thx/client/alfabet/willhillcards/7c.bmp");
        cards[47] = new Img("c:/thx/client/alfabet/willhillcards/6c.bmp");
        cards[48] = new Img("c:/thx/client/alfabet/willhillcards/5c.bmp");
        cards[49] = new Img("c:/thx/client/alfabet/willhillcards/4c.bmp");
        cards[50] = new Img("c:/thx/client/alfabet/willhillcards/3c.bmp");
        cards[51] = new Img("c:/thx/client/alfabet/willhillcards/2c.bmp");
    }

    public static void main(String[] args) {

    }

    List<Integer> checkCards() {

        List<Integer> l = new ArrayList<Integer>();
        Rectangle ar;
        if (agent != null) {
            ar = agent;
        } else {
            ar = new Rectangle(0, 0, area.width - 10, area.height - 50);
        }
        for (int i = 0; i < cards.length; i++) {
            Point r = engine.getImage().includes(ar, cards[i]);
            if (r != null) {
                l.add(i + 1);
            //System.out.println("found: " + i);
            }

        }
        return l;
    }

    String getCards(List<Integer> l) {
        if (l.size() == 2) {
            return "c1=" + l.get(0) + "&c2=" + l.get(1);
        }
        if (l.size() == 4) {
            return "c1=" + l.get(0) + "&c2=" + l.get(1) + "&c3=" + l.get(2) + "&c4=" + l.get(3);
        }
        return null;
    }

    void holeCards() {
        if (!checked_cards) {
            if (agent != null) {
                String s = getCards(checkCards());
                if (s != null) {
                    sendAndRead("holecards", s);
                    checked_cards = true;
                }
            }
        }
    }

    @Override
    void synchronize() throws Exception {
        synchron = engine.getImage().findRectangle(398 - 67, 502 - 483, Img.Black);
        if (synchron == null) {
            throw new Exception("Brak synchronizacji");
        } else {
            chatArea = new Rectangle(synchron.x, synchron.y + 20, 380 - 67, 575 - 503);
            Point p = engine.getImage().includes(area,
                    new Img("c:/thx/client/alfabet/willhillcards/Grabol.bmp"));
            if (p == null) {
                p = engine.getImage().includes(area, agentAction);
            }
            if (p != null) {
                //2 gorne polozenia
                if (p.y < 100) {
                    agent = new Rectangle(p.x - 50, p.y + 10, 110, 60);
                } else {
                    agent = new Rectangle(p.x - 50, p.y - 90, 110, 60);
                }
                engine.logs("agent=" + agent);
            } else {
                agent = null;
            }
        }
        engine.logs("Synch [" + synchron.x + "," + synchron.y + "]");
    }

    int rank2val( char r) {
        switch (r) {
            case '2':
                return 13;
            case '3':
                return 12;
            case '4':
                return 11;
            case '5':
                return 10;
            case '6':
                return 9;
            case '7':
                return 8;
            case '8':
                return 7;
            case '9':
                return 6;
            case 'T':
                return 5;
            case 'J':
                return 4;
            case 'Q':
                return 3;
            case 'K':
                return 2;
            case 'A':
                return 1;

            default:
                return -100;
        }
    }

    int color2val( char c) {
        switch (c) {
            case 'c':
                return 3;
            case 'd':
                return 2;
            case 'h':
                return 1;
            default:
                return 0;
        }
    }

    @Override
    String newSentence( String sent) {
        int dl = sent.length();
        int pocz;
        String name,
         amount;

        if (sent.indexOf("Dealer:Startingnewhand") > -1) {
            String id = sent.substring(24, dl);
            sendAndRead("newgame", "id=" + id);
        }

        if (sent.indexOf("Dealer:DealingHoleCards") > -1) {
            checked_cards = false;
        }

        // Dealer:Playerpostssmallblind$0.25
        pocz = sent.indexOf("postssmallblind");
        if (pocz > -1) {
            name = sent.substring(7, pocz);
            amount = sent.substring(sent.indexOf("blind") + 6, dl);
            //send("newgame", "");
            checked_cards = false;
            return sendAndRead("smallblind", "name=" + name + "&amount=" + amount);
        }

        pocz = sent.indexOf("postsbigblind");
        if (pocz > -1) {
            name = sent.substring(7, pocz);
            amount = sent.substring(sent.indexOf("blind") + 6, dl);
            return sendAndRead("bigblind", "name=" + name + "&amount=" + amount);

        }

        if (sent.endsWith("folds")) {
            holeCards();
            pocz = sent.indexOf("folds");
            name = sent.substring(7, pocz);
            return sendAndRead("fold", "name=" + name);
        }

        pocz = sent.indexOf("calls");
        if (pocz > -1) {
            holeCards();
            name = sent.substring(7, pocz);
            amount = sent.substring(pocz + 6, dl);
            return sendAndRead("call", "name=" + name + "&amount=" + amount);

        }

        if (sent.endsWith("checks")) {
            holeCards();
            pocz = sent.indexOf("checks");
            name = sent.substring(7, pocz);
            return sendAndRead("call", "name=" + name + "&amount=0");

        }

        pocz = sent.indexOf("raisesto");
        if (pocz > -1) {
            holeCards();
            name = sent.substring(7, pocz);
            amount = sent.substring(sent.indexOf("raisesto") + 9, dl);
            return sendAndRead("raise", "name=" + name + "&amount=" + amount);

        }

        pocz = sent.indexOf("raises");
        if (pocz > -1) {
            holeCards();
            name = sent.substring(7, pocz);
            amount = sent.substring(sent.indexOf("raises") + 7, dl);
            return sendAndRead("bet", "name=" + name + "&amount=" + amount);

        }

        pocz = sent.indexOf("bets");
        if (pocz > -1 & !sent.startsWith("Dealer:Returneduncalledbets")) {
            holeCards();
            name = sent.substring(7, pocz);
            amount = sent.substring(pocz + 5, dl);
            return sendAndRead("bet", "name=" + name + "&amount=" + amount);

        }

        if (sent.endsWith("isall-in")) {
            holeCards();
            pocz = sent.indexOf("isall-in");
            name = sent.substring(7, pocz);
            return sendAndRead("allin", "name=" + name);

        }

        if (sent.startsWith("Dealer:DealingFlop")) {
            char r1 = sent.charAt(dl - 3),
             k1 = sent.charAt(dl - 2);
            int c1 = 13 * color2val(k1) + rank2val(r1);
            char r2 = sent.charAt(dl - 5),
             k2 = sent.charAt(dl - 4);
            int c2 = 13 * color2val(k2) + rank2val(r2);
            char r3 = sent.charAt(dl - 7),
             k3 = sent.charAt(dl - 6);
            int c3 = 13 * color2val(k3) + rank2val(r3);
            return soundMessage(sendAndRead("flop", "c1=" + c1 + "&c2=" + c2 + "&c3=" + c3));
        }

        if (sent.startsWith("Dealer:DealingTurn")) {
            char r = sent.charAt(dl - 3),
             k = sent.charAt(dl - 2);
            //System.out.println("======"+r+c);
            int c1 = 13 * color2val(k) + rank2val(r);
            return sendAndRead("turn", "c1=" + c1);
        }

        if (sent.startsWith("Dealer:DealingRiver")) {
            char r = sent.charAt(dl - 3),
             k = sent.charAt(dl - 2);
            //System.out.println("======"+r+c);
            int c1 = 13 * color2val(k) + rank2val(r);
            return sendAndRead("river", "c1=" + c1);
        }

        return null;
    }

    //TODO przeniesc do Casino
    @Override
    void Action() {
        holeCards();
        if (!checked_cards) {
            engine.getImage().write(String.valueOf(System.currentTimeMillis()));
        }
        String res = sendAndRead("action", null);
        engine.logs("THX:  " + res);
        String polecenia[] = res.split(" ");
        String pol = polecenia[0];
        if (polecenia.length > 1) {
            playSound(polecenia[1]);
            try {
                Thread.sleep(3000);
            } catch ( InterruptedException ex) {
                Logger.getLogger(WillHill.class.getName()).log(Level.SEVERE, null, ex);
            }
        }

        if (!engine.isAutoPlay()) {
            //engine.logs("AutoPlay Off");
            return;
        }

        if (pol.equals("fold")) {
            doFold();
        }
        if (pol.equals("call")) {
            doCall();
            doCheck();
        }
        if (pol.equals("bet")) {
            doBet();
        }
        if (pol.equals("potbet1")) {
            doPotBet();
        }
        if (pol.equals("potbet2")) {
            doPotBet2();
        }

        if (pol.equals("sitout")) {
            doFold();
            doSitOut();
        }

        //parkowanie myszy:
        engine.robot.mouseMove(synchron.x + 793, synchron.y + 116);

    }

    @Override
    boolean isAction() {
        if (agent != null) {
            int y = agent.y < 50 ? 0 : agent.y + 50;
            Rectangle obszar = new Rectangle(agent.x, y, agent.width, agent.height + 100);
            return engine.getImage().includes(obszar, agentAction) != null;
        }
        return false;
    }

    @Override
    void registerClient() {
        sendAndRead("registerclient", "agent=Grabol&casino=willhill&kind=" + kind);
    }

    @Override
    void doFold() {
        engine.click(synchron.x + 406, synchron.y + 542 - 481);
    }

    @Override
    void doCall() {
        engine.click(synchron.x + 605 - 71, synchron.y + 542 - 481);
    }

    @Override
    void doCheck() {
        doCall();
    }

    @Override
    void doBet() {
        engine.click(synchron.x + 734 - 71, synchron.y + 542 - 481);
    }

    @Override
    void doPotBet() {
        engine.click(synchron.x + 773 - 71, synchron.y + 492 - 481);
        doBet();
    }

    @Override
    void doPotBet2() {
        engine.click(synchron.x + 622 - 71, synchron.y + 496 - 481);
        engine.click(synchron.x + 622 - 71, synchron.y + 496 - 481);
        engine.click(synchron.x + 622 - 71, synchron.y + 496 - 481);
        doBet();
    }

    @Override
    void doSitOut() {
        throw new UnsupportedOperationException("Not supported yet.");
    }
}
