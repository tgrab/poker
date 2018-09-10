import java.awt.image.BufferedImage;
import java.util.*;
import java.io.*;
import javax.swing.*;


//-----------------------------------------------------------------
class OCRImg extends Img {
    
    String code;
    
    OCRImg(Img img,String code) {
        super(img);
        this.code=code;
    }
    
    boolean theSame(Img im) {
        BufferedImage b=im.image;
        if(width!=im.width || height!=im.height) return false;
        for(int c=0;c<width;c++)
            for(int r=0;r<height;r++)
                if(image.getRGB(c,r)!=b.getRGB(c,r)) return false;
        return true;
    }
    
}
//-----------------------------------------------------------------


//-----------------------------------------------------------------
public class Alphabet {
    
    ArrayList<OCRImg> letters=new ArrayList<OCRImg>();
    boolean teaching=true;
    String dir;

  
    public Alphabet(String d) {
        dir=d;
        File fdir=new File(d);
        if (!fdir.exists()) {
            JOptionPane.showMessageDialog(null,"Brak katalogu slownika!");
            System.exit(0);
        }        
        read();          
    }
    
    void write() {
        try {
            PrintWriter wy=new PrintWriter(new FileWriter(dir+"/letters.txt"));
            int licznik=1;
            for (OCRImg i :letters) {
                i.write(dir+"/"+licznik);
                wy.println(licznik+".bmp "+i.code);
                licznik++;
            }
            wy.close();
        } catch (IOException ex) {
            ex.printStackTrace();
        }
    }
    
    void read() {
        try {
            letters=new ArrayList<OCRImg>();
            BufferedReader we=new BufferedReader(new FileReader(dir+"/letters.txt"));
            String w=we.readLine();
            String nazwa,kod;
            String tab[];
            while (w!=null) {
                tab=w.split(" ");
                nazwa=tab[0];
                kod=tab[1];
                letters.add(new OCRImg(new Img(javax.imageio.ImageIO.read(new File(dir+"/"+nazwa))),kod));
                w=we.readLine();
            }
            we.close();
        }catch(Exception w){w.printStackTrace();}
    }
    
    String teach(Img im) {
        String result=recognize(im);
        if (result!=null) return result;
        if (!teaching) return "*";
        result=JOptionPane.showInputDialog(null,new ImgView(im),"Nieznany obraz",JOptionPane.OK_OPTION);
        if (result!=null)
            letters.add(new OCRImg(im,result));
        return result;
    }
    
    String recognize(Img im) {
        for (OCRImg i : letters) if (i.theSame(im)) return i.code;
        return null;
    }
    
}

