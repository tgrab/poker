/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */
package history;

import java.io.ByteArrayInputStream;
import java.io.IOException;
import java.util.logging.Level;
import java.util.logging.Logger;
import javax.xml.parsers.ParserConfigurationException;
import javax.xml.parsers.SAXParser;
import javax.xml.parsers.SAXParserFactory;
import java.sql.*;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.LinkedList;
import java.util.List;
import org.xml.sax.Attributes;
import org.xml.sax.SAXException;
import org.xml.sax.helpers.DefaultHandler;

public class PrimaLog {

    public int action_nr = 0;
    public String gametype = "";
    public String id;
    public String date;
    public String tablename;
    HashMap<String, String> names = new HashMap<String, String>();
    HashMap<String, String> balance = new HashMap<String, String>();
    HashMap<String, String> endbalance = new HashMap<String, String>();
    public List<PlayerCards> playerCards;
    public int holecards[] = new int[4];
    int holeindex = 0;
    public int flop[] = new int[3];
    int flopindex = 0;
    public int turn;
    public int river;
    boolean seats = false;
    public boolean dealcards = false;
    boolean flopcards = false;
    boolean turncards = false;
    boolean rivercards = false;
    boolean showcards = false;
    PlayerCards playerCardsTemp;
    public List<Action> history = new ArrayList<Action>();
    private float bigBlindValue;
    public String xmlSource;

    public static String array2sexpr(int[] a) {
        if (a == null) {
            return "()";
        }
        StringBuffer s = new StringBuffer("( ");
        if (a[0] > 0) {
            for (int i : a) {
                s.append(i + " ");
            }
        }
        s.append(" )");
        return s.toString();
    }

    public static String list2sexpr(List<Integer> a) {
        if (a == null) {
            return "()";
        }
        StringBuffer s = new StringBuffer("( ");
        if (a.size() > 0) {
            for (int i : a) {
                s.append(i + " ");
            }
        }
        s.append(" )");
        return s.toString();
    }

    public String table2sexpr() {
        StringBuffer s = new StringBuffer("( ");
        if (river > 0) {
            s.append(river + " ");
        }
        if (turn > 0) {
            s.append(turn + " ");
        }
        if (flop[0] > 0) {
            for (int i : flop) {
                s.append(i + " ");
            }
        }
        s.append(" )");
        return s.toString();
    }

    public String sexprDesc() {
        StringBuffer s = new StringBuffer("(make-log ");
        s.append(" :id ");s.append(id);
        s.append(" :tablename \"");s.append(tablename);s.append("\"");
        s.append(" :date \"");s.append(date);s.append("\"\n");
        s.append(" :history '(");
        for (Action a: history) {
            s.append(a.toString());
        }
        s.append(" )");
        s.append(" :holecards '" + array2sexpr(holecards));
        s.append(" :tablecards '" + table2sexpr());
        if (playerCards != null) {
            s.append(" :shown '(");
            for (PlayerCards p : playerCards) {
                if (p.cards!=null) {
                    s.append(" ( \""+p.name+"\" . ");
                    s.append(list2sexpr(p.cards));
                    s.append(" )");
                }
            }
            s.append(" )");
        }
        s.append(" )");
        return s.toString();
    }

    class PrimaHandler extends DefaultHandler {

        @Override
        public void startElement(String uri, String localName, String qName, Attributes atr) throws SAXException {
            //System.out.println("el:"+uri+localName+qName);
            if (qName.equals("Game")) {
                id = atr.getValue("id");
                date = atr.getValue("date");
                tablename = atr.getValue("tablename");
                gametype = atr.getValue("gametype");
            }
            if (!gametype.equals("Omaha")) {
                return;
            }
            if (qName.equals("Seats")) {
                seats = true;
            }
            if (qName.equals("Seat") && seats) {
                //System.out.println(atr.getValue("num")+" => "+   atr.getValue("alias"));
                names.put(atr.getValue("num"), atr.getValue("alias"));
                balance.put(atr.getValue("num"), atr.getValue("balance"));
                endbalance.put(atr.getValue("num"), atr.getValue("endbalance"));
            }

            if (qName.equals("Card")) {
                if (showcards) {
                    playerCardsTemp.cards.add(decodeCard(atr.getValue("value"), atr.getValue("suit")));
                }
                if (dealcards) {
                    holecards[holeindex++] = decodeCard(atr.getValue("value"), atr.getValue("suit"));
                }
                if (flopcards) {
                    flop[flopindex++] = decodeCard(atr.getValue("value"), atr.getValue("suit"));
                }
                if (turncards) {
                    turn = decodeCard(atr.getValue("value"), atr.getValue("suit"));
                }
                if (rivercards) {
                    river = decodeCard(atr.getValue("value"), atr.getValue("suit"));
                }
            }

            if (qName.equals("Action")) {
                String typ = atr.getValue("type");
                if (typ.equals("SmallBlind")) {
                    Action a = new Action("sb", names.get(atr.getValue("seat")), atr.getValue("value"));
                    history.add(a);
                }
                if (typ.equals("BigBlind")) {
                    String bb = atr.getValue("value");
                    bigBlindValue = Float.parseFloat(bb);
                    Action a = new Action("bb", names.get(atr.getValue("seat")), bb);
                    history.add(a);
                }
                if (typ.equals("Call")) {
                    Action a = new Action("call", names.get(atr.getValue("seat")), atr.getValue("value"));
                    history.add(a);
                }
                if (typ.equals("Check")) {
                    Action a = new Action("check", names.get(atr.getValue("seat")), null);
                    history.add(a);
                }
                if (typ.equals("Fold")) {
                    Action a = new Action("fold", names.get(atr.getValue("seat")), null);
                    history.add(a);
                }
                if (typ.equals("Raise")) {
                    Action a = new Action("raise", names.get(atr.getValue("seat")), atr.getValue("value"));
                    history.add(a);
                }
                if (typ.equals("Bet")) {
                    Action a = new Action("bet", names.get(atr.getValue("seat")), atr.getValue("value"));
                    history.add(a);
                }
                if (typ.equals("AllIn")) {
                    Action a = new Action("allin", names.get(atr.getValue("seat")), atr.getValue("value"));
                    history.add(a);
                }
                if (typ.equals("DealCards")) {
                    dealcards = true;
                }
                if (typ.equals("ShowCards") | typ.equals("MuckCards")) {
                    if (names.get(atr.getValue("seat")).equals("grabola")) {
                        return;
                    }
                    playerCardsTemp = new PlayerCards();
                    playerCardsTemp.cards = new LinkedList<Integer>();
                    playerCardsTemp.name = names.get(atr.getValue("seat"));
                    showcards = true;
                }
                if (typ.equals("DealFlop")) {
                    flopcards = true;
                    Action a = new Action("flop", null, null);
                    history.add(a);
                }
                if (typ.equals("DealTurn")) {
                    turncards = true;
                    Action a = new Action("turn", null, null);
                    history.add(a);
                }
                if (typ.equals("DealRiver")) {
                    rivercards = true;
                    Action a = new Action("river", null, null);
                    history.add(a);
                }
            }
        }

        @Override
        public void endElement(String uri, String localName, String qName) throws SAXException {
            if (qName.equals("Seats")) {
                seats = false;
            }
            if (qName.equals("Action")) {
                dealcards = false;
                flopcards = false;
                turncards = false;
                rivercards = false;
                if (showcards) {
                    if (playerCardsTemp.cards.size() > 0) {
                        if (playerCards == null) {
                            playerCards = new LinkedList<PlayerCards>();
                        }
                        playerCards.add(playerCardsTemp);
                    }
                }
                showcards = false;
            }
        }
    }

    public Double balance(String name) {
        String pos = null;
        for (String nr : names.keySet()) {
            if (names.get(nr).equals(name)) {
                pos = nr;
            }
        }
        if (pos == null) {
            return null;
        }
        if (Double.parseDouble(balance.get(pos)) < 0.01) {
            return 0.0;
        } else {
            return Math.floor(100 * (Double.parseDouble(endbalance.get(pos)) - Double.parseDouble(balance.get(pos))) / bigBlindValue) / 100;
        }
    }

    int decodeCard(String rank, String suit) {
        int r = 0, s = 0;
        char cr = rank.charAt(0);
        char cs = suit.charAt(0);
        switch (cr) {
            case '2':
                r = 13;
                break;
            case '3':
                r = 12;
                break;
            case '4':
                r = 11;
                break;
            case '5':
                r = 10;
                break;
            case '6':
                r = 9;
                break;
            case '7':
                r = 8;
                break;
            case '8':
                r = 7;
                break;
            case '9':
                r = 6;
                break;
            case '1':   //rank byl rowny "10"
                r = 5;
                break;
            case 'J':
                r = 4;
                break;
            case 'Q':
                r = 3;
                break;
            case 'K':
                r = 2;
                break;
            case 'A':
                r = 1;
                break;
        }
        switch (cs) {
            case 'c':
                s = 3;
                break;
            case 'd':
                s = 2;
                break;
            case 'h':
                s = 1;
                break;
            case 's':
                s = 0;
        }

        return 13 * s + r;
    }

    public void parsuj(String xml) {
        xmlSource = xml;
        try {
            PrimaHandler h = new PrimaHandler();
            SAXParser sax = SAXParserFactory.newInstance().newSAXParser();
            sax.parse(new ByteArrayInputStream(xml.getBytes()), h);
        } catch (IOException ex) {
            Logger.getLogger(PrimaLog.class.getName()).log(Level.SEVERE, null, ex);
        } catch (ParserConfigurationException ex) {
            Logger.getLogger(PrimaLog.class.getName()).log(Level.SEVERE, null, ex);
            System.out.println("XML:" + xml);
        } catch (SAXException ex) {
            Logger.getLogger(PrimaLog.class.getName()).log(Level.SEVERE, null, ex);
            System.out.println("XML:" + xml);
        }
    }

    @Override
    public String toString() {
        Double bal = balance("grabola");
        String sbal = (bal != null) ? bal + "" : "--";
        return id + "  at  " + date + "  bal: " + sbal + "$   " + tablename;
    }

    public void show() {
        System.out.println(this);
        for (Action s : history) {
            System.out.println(s);
        }
        System.out.println("Holecards");
        for (int c : holecards) {
            System.out.print(c + " ");
        }
    }

    public Action nextAction() {
        if (action_nr == history.size()) {
            return new Action("FINISHED", null, null);
        }
        return history.get(action_nr++);
    }

    public static void main(String a[]) throws Exception {
        Class.forName("org.sqlite.JDBC");
        //Connection conn = DriverManager.getConnection("jdbc:sqlite:c:\\History.dat");
        Connection conn = DriverManager.getConnection("jdbc:sqlite:/media/c/MicroGaming/Poker/PurpleLoungeMPP/GameHistory.dat");
        Statement st = conn.createStatement();
        //ResultSet rs = st.executeQuery("select * from HandHistory limit 1,2");
        ResultSet rs = st.executeQuery("SELECT * FROM HandHistory ORDER BY HandDate DESC LIMIT 22,23");
        rs.next();
        String dump = rs.getString("XMLDump");
        //System.out.println(dump);
        System.out.println("------------------------------------------");
        PrimaLog log = new PrimaLog();
        log.parsuj(dump);
        System.out.println(log);
        System.out.println(log.names + " " + log.balance + " " + log.endbalance);
        for (Action s : log.history) {
            System.out.println(s);
        }
        System.out.println("Holecards");
        for (int c : log.holecards) {
            System.out.print(c + " ");
        }
        System.out.println("\nFlop");
        for (int c : log.flop) {
            System.out.print(c + " ");
        }
        System.out.println("\nBalance" + log.balance("grabola"));
        if (log.playerCards != null) {
            for (PlayerCards c : log.playerCards) {
                for (int i : c.cards) {
                    System.out.print(i + " ");
                }
                System.out.println();
            }
        }

    }
}
