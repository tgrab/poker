/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */
package history;

/**
 *
 * @author tgrab
 */
public class Action {

   public String type;
   public String name;
   public  String amount;

    public Action(String type, String name, String amount) {
        this.type = type;
        this.name = name;
        this.amount = amount;
    }

    @Override
    public String toString() {
        if (name==null) return "(" + type +")\n";
        if (amount==null) return "(" + type + " \"" + name+"\")";
        return "(" + type + " \"" + name + "\" " + amount + ")";
    }
}
