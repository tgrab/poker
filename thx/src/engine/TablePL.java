/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */
package engine;

import java.awt.Rectangle;
import java.util.Properties;
import javax.swing.JOptionPane;

/**
 *
 * @author tgrab
 */
public class TablePL extends Table {

    String memoSBlind = "0.1";
    String memoBBlind = "0.2";
    int chatWidth;



    public TablePL(Screen s,Rectangle area, Properties p) {
        super(s,area,p);
        writeLog(1, "Table PurpleLounge created@"+area);
        chatWidth=189;
    }

    @Override
    void installChatArea() {
        if (synchron == null) {
            synchronize();
        }
        chatArea = new Rectangle(synchron.x, synchron.y, chatWidth, 480);
        //chatArea = new Rectangle(synchron.x, synchron.y+368, 189, 112);
    }

    @Override
    public Img getChat() {
        if (chatArea == null) {
            installChatArea();
        }
        Img res = new Img(Screen.image.subImage(chatArea));
        res.im2BW();
        return res;
    }

    @Override
    void installAlphabet() {
        alphabet = new Alphabet("alfabet_pl");
    }

    @Override
    public void synchronize() {
        synchron = null;
        //szukamy bialego prostokata o wym. 180x60
        synchron = Screen.image.znajdzBialyProstokat(chatWidth, 479, area);
        if (synchron != null) {
            installChatArea();
            writeLog(1, "Synchronizacja: " + synchron);
        } else {
            JOptionPane.showMessageDialog(this, "Brak synchronizacji", "Błąd", JOptionPane.WARNING_MESSAGE);
        }
    }

    @Override
    boolean isAction() {
        if (synchron == null) {
            return false;
        }
        if (Screen.image.image.getRGB(synchron.x - 265, synchron.y + 530) == Img.White) {
            return true;
        }
        return false;
    }

    @Override
    void actionFold() {
        screen.click(synchron.x - 523, synchron.y + 485);
    }

    @Override
    void actionCall() {
        screen.click(synchron.x - 406, synchron.y + 485);
    }

    @Override
    void actionBet() {
        screen.click(synchron.x - 294, synchron.y + 485);
    }

    @Override
    void actionPotBet1() {
        screen.click(synchron.x - 334, synchron.y + 554);
        actionBet();
    }

    @Override
    void actionPotBet2() {
        screen.click(synchron.x - 382, synchron.y + 554);
        actionBet();
    }

    private void fShow(String kod, String s1, String s2) {
        System.out.println(kod + ": " + s1 + " " + s2);
    }

    int decodeCard(String card) {
        int r = 0, s = 0;
        char cr = card.charAt(0);
        char cs = card.charAt(1);
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
            case 'T':
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

    private String remove10s(String cards) {
        return cards.replaceAll("10", "T");
    }

    String oldsentence;

    @Override
    void newSentence(String s) {

        if (oldsentence!=null) {
            s=oldsentence+s;
            oldsentence=null;
            newSentence(s);
        }

        int k;
        int dl = s.length();

        //>possu23postedsmallblind(0.10)
        k = s.indexOf("postedsmallblind");
        if (k > -1) {
            newGame("0");
            if (dl > k + 17) {
                memoSBlind = s.substring(k + 17, dl - 1);
            }
            smallBlind(s.substring(1, k), memoSBlind);
        }

        //>matrix32postedbigblind(0.04)
        k = s.indexOf("postedbigblind");
        if (k > -1) {
            if (dl > k + 15) {
                memoBBlind = s.substring(k + 15, dl - 1);
            }
            bigBlind(s.substring(1, k), memoBBlind);
        }

        //>Game#2767507497starting.
        k = s.indexOf(">Game#");
        if (k > -1) {
            game.id = s.substring(6, 16);
            evalLisp("(setf (id *game*) " + game.id + " )");
        }

        //>bheaversedgecalledfor1.25
        k = s.indexOf("calledfor");
        if (k > -1) {
            call(s.substring(1, k), s.substring(k + 9, dl));
        }

        //>vps4betfor3
        k = s.indexOf("betfor");
        if (k > -1) {
            bet(s.substring(1, k), s.substring(k + 6, dl));
        }

        k = s.indexOf("raisedfor");
        if (k > -1) {
            raise(s.substring(1, k), s.substring(k + 9, dl));
        }

        k = s.indexOf("wentallinfor");
        if (k > -1) {
            if (k+12>=dl) {
              oldsentence=s;
              return;
            }
            else
                allIn(s.substring(1, k), s.substring(k + 12, dl));
        }

        //>bheaversedgechecked
        k = s.indexOf("checked");
        if (k > -1) {
            call(s.substring(1, k), "0");
        }

        //>possu23folded
        k = s.indexOf("folded");
        if (k > 1) {  //moze byc samo folded z pop. linii
            fold(s.substring(1, k));
        }

        //>DealingtheFlop(Ac6hQh)
        k = s.indexOf(">DealingtheFlop");
        if (k > -1) {
            String cards = remove10s(s.substring(k + 16, dl - 1));
            //System.out.println(cards);
            flop(decodeCard(cards.substring(0, 2)),
                    decodeCard(cards.substring(2, 4)),
                    decodeCard(cards.substring(4, 6)));
        }

        k = s.indexOf(">DealingtheTurn");
        if (k > -1) {
            String cards = remove10s(s.substring(k + 16, dl - 1));
            //System.out.println(cards);
            turn(decodeCard(cards.substring(0, 2)));
        }

        k = s.indexOf(">DealingtheRiver");
        if (k > -1) {
            String cards = remove10s(s.substring(k + 17, dl - 1));
            //System.out.println(cards);
            river(decodeCard(cards.substring(0, 2)));
        }

        k = s.indexOf(">DealingHoleCards");
        if (k > -1) {
            if (dl==17) return;
            String cards = remove10s(s.substring(k + 18, dl - 1));
            //System.out.println("Holecards"+cards);
            holeCards(decodeCard(cards.substring(0, 2)),
                    decodeCard(cards.substring(2, 4)),
                    decodeCard(cards.substring(4, 6)),
                    decodeCard(cards.substring(6, 8)));
        }
        k = s.indexOf("grabolayouhave10seconds");
        if (k > -1) {
            //to oznacza ze mam allina
            screen.sayIt("Ten seconds");
            String a = evalLisp("(action)");
            if (!withAutoPlay) {
                return;
            }
            if (a.equals("fold")) actionFold();
            else actionCall();
        }

    }

    @Override
    String agentName() {
       return "grabola";
    }

    @Override
    void actionSitOut() {
        screen.click(synchron.x +33-816, synchron.y + 540-54);
    }
}
