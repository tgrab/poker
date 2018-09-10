//bedzie uzywane w przyszlosci
//dla kasyn nie zapisujacych na czacie stanu gry
//bedzie odpowiednikiem kodu Lispa

public class Game {
    byte kind;
    byte[] cards;
    byte[] table_cards;
    
    public Game() {        
    }

    public Game(byte kind) {
        this.kind = kind;
    }
    
    public void holecards(byte c1,byte c2)  {
        cards=new byte[2];
        cards[0]=c1;cards[1]=c2;
    }
   
    public void holecards(byte c1,byte c2,byte c3,byte c4)  {
        cards=new byte[4];
        cards[0]=c1;cards[1]=c2;  cards[2]=c3;cards[3]=c4;
    }    
    
    
    
}