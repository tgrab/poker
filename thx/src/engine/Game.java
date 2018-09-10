package engine;

public class Game {

    public String id = "";
    public String date;
    public int freeseat;
    public float pot,  stake;
    public Player[] players;
    public int cards[];
    public int table_cards[];
    float blind=0.2f;

    public Game(String id) {
        this.id = id;
        players = new Player[10];
    }

    public Player getPlayer(String name) {
        for (Player p : players) {
            if (p!=null && p.name.equals(name)) {
                return p;
            }
        }
        return null;
    }

    public boolean playerCommited(String name) {
        Player p=getPlayer(name);
        if (p!=null) {
            if (p.comittment()/blind > 4) return true;
        }
        return false;
    }

    private void insertPlayer(String name) {
        if (table_cards == null && freeseat <= 9 && getPlayer(name) == null) {
            players[freeseat] = new Player(freeseat, name);
            freeseat++;
        }
    }

    public void smallBlind(String name, float amount) {
        if (freeseat>0) return;
        pot = stake = amount;
        insertPlayer(name);
        Player p=getPlayer(name);
        p.lastAction=String.format("SB %.2f", amount);
        p.balance=-amount;
    }

    public void bigBlind(String name, float amount) {
        if (freeseat!=1) return;
        blind=stake = amount;
        pot += stake;
        insertPlayer(name);
        Player p=getPlayer(name);
        p.lastAction=String.format("BB %.2f", amount);
        p.balance=-amount;
    }

    public void holecards(int c1, int c2, int c3, int c4) {
        cards = new int[4];
        cards[0] = c1;
        cards[1] = c2;
        cards[2] = c3;
        cards[3] = c4;

    }

    private void roundRules() {
        for (Player p:players) {
            if (p!=null) {
                p.totalBalance+=p.balance;
                p.balance=0;
                p.lastAction="";
            }
        }
    }

    public void flop(int c1, int c2, int c3) {
        if (table_cards!=null || freeseat==0) return;
        table_cards = new int[3];
        table_cards[0] = c1;
        table_cards[1] = c2;
        table_cards[2] = c3;
        roundRules();
    }

    public void turn(int c1) {
        if (table_cards==null || table_cards.length!=3) return;
        int tc[] = new int[4];
        tc[0] = table_cards[0];
        tc[1] = table_cards[1];
        tc[2] = table_cards[2];
        tc[3] = c1;
        table_cards = tc;
        roundRules();
    }

    public void river(int c1) {
        if (table_cards==null || table_cards.length!=4) return;
        int tc[] = new int[5];
        tc[0] = table_cards[0];
        tc[1] = table_cards[1];
        tc[2] = table_cards[2];
        tc[3] = table_cards[3];
        tc[4] = c1;
        table_cards = tc;
         roundRules();
    }

    public void call(String name, float amount) {
        insertPlayer(name);
        Player p=getPlayer(name);
        if (p==null) return;
        if (amount!=0)p.lastAction=String.format("Call %.2f", amount);
        else p.lastAction="Check";
        p.balance+=-amount;
        pot += amount;
    }

    public void bet(String name, float amount) {
        insertPlayer(name);
        Player p=getPlayer(name);
        if (p==null) return;
        p.lastAction=String.format("Bet %.2f", amount);
        p.balance=-amount;
        stake = amount;
        pot += amount;
    }

    public void raise(String name, float amount) {
        insertPlayer(name);
        Player p=getPlayer(name);
        if (p==null) return;
        p.lastAction=String.format("Raise %.2f", amount);
        p.balance=-amount;
        stake = amount;
        pot += amount;
    }

    public void allIn(String name, float amount) {
        insertPlayer(name);
        Player p=getPlayer(name);
        if (p==null) return;
        p.lastAction=String.format("AllIn %.2f", amount);
        p.balance=-amount;
        pot += amount;
    }

    public void allIn(String name) {
        insertPlayer(name);
        Player p=getPlayer(name);
        if (p==null) return;
        p.lastAction="AllIn";
    }

    public void fold(String name) {
        insertPlayer(name);
        Player p=getPlayer(name);
        if (p==null) return;
        p.active = false;
        p.lastAction="Fold";
    }
}
