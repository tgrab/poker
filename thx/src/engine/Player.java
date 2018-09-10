package engine;


public class Player {

    int seat;
    String name;
    boolean active = true;
    public float balance;
    float totalBalance;
    String lastAction="";

    public Player(int seat, String name) {
        this.seat = seat;
        this.name = name;
    }

    public float comittment() {
        return -balance-totalBalance;
    }

}
