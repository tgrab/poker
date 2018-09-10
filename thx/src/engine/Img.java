package engine;

import java.util.*;
import java.io.*;
import javax.swing.*;
import java.awt.*;
import java.awt.color.*;
import java.awt.image.*;

//klasy do wyswietlania i analizy obrazow
//-----------------------------------------------------------------
class ImgView  extends JPanel {

    Img img;

    ImgView(Img img) {
        this.img=img;
        setPreferredSize(new Dimension(img.width,img.height));
    }

    @Override
    public void paintComponent(Graphics g) {
        Graphics2D gr=(Graphics2D)g;
        super.paintComponent(gr);
        if (img!=null) gr.drawImage(img.image,null,0,0);
    }
}


//-----------------------------------------------------------------
public class Img {

    BufferedImage image;
    int width,height;
    static int White=Color.white.getRGB();
    static int Black=Color.black.getRGB();

    public Img() {
        //image=new BufferedImage(0, 0, 0);
    }

    Img(Img i) {
        if (i.image!=null)
            setImage(i.image);
    }

    Img(BufferedImage i) {
        setImage(i);
    }

    Img (String file) {
        read(file);
    }



    void setImage(BufferedImage i) {
        image=i;
        width=i.getWidth();
        height=i.getHeight();
    }

    public void read(String file) {
        try {
            setImage(javax.imageio.ImageIO.read(new File(file)));
        }catch(Exception wyj) {wyj.printStackTrace();}
    }

    public static void write(BufferedImage im,String nazwa) {
        try {
            javax.imageio.ImageIO.write(im,"bmp",new File(nazwa+".bmp"));
        }catch(Exception wyj) {wyj.printStackTrace();}
    }


    public void write(String nazwa) {
        write(image,nazwa);
    }

    BufferedImage subImage(Rectangle r) {
        return image.getSubimage(r.x,r.y,r.width,r.height);
    }




    public void show() {
        JFrame f=new JFrame();
        f.add(new ImgView(this));
        f.pack();
        f.setVisible(true);
    }

    //---------------------
    // synchronizacja PL:
    Point znajdzBialyProstokat(int szer,int wys,Rectangle view) {
        int xp=view.x;
        int yp=view.y;
        //przechodzi caly obszar
        for (int x=xp;x<xp+view.width-szer;x++){
            for (int y=yp;y<yp+view.height-wys;y++) {
                //System.out.println("zBP "+x+" "+y);
               if (isRectangle(x,y,szer,wys,White)) return new Point(x,y);
            }
         }
        return null;
    }

    //-------------------------
    //filtry:
    //-------------------------

    void im2BW() {
        //tworzy obraz czarno-bialy
        for (int x=0;x<width;x++)
            for (int y=0;y<height;y++) {
            int kolor=image.getRGB(x,y);
            Color c=new Color(kolor);
            if (c.getRed()>250 & c.getGreen()>250 & c.getBlue()>250)
                image.setRGB(x,y,Color.white.getRGB());
            else
                image.setRGB(x,y,Color.black.getRGB());
            }
    }



    boolean noColorInRow(int row,int column,int length,int color) {
        int end=column+length;
        if (column+length>width) end=width;
        for(int kol=column; kol<end;kol++)
            if (image.getRGB(kol,row)==color) return false;
        return true;
    }

    boolean noColorInCol(int column,int row,int length,int color) {
        int end=row+length;
        if (row+length>height) end=height;//!!!!
        for(int r=row; r<end;r++)
            if (image.getRGB(column,r)==color) return false;
        return true;
    }

    boolean existsColorInRow(int row,int column,int length,int color) {
        int end=column+length;
        if (column+length>width) end=width;
        for(int kol=column; kol<end;kol++)
            if (image.getRGB(kol,row)==color) return true;
        return false;
    }

    boolean existsColorInCol(int column,int row,int length,int color) {
        int end=row+length;
        if (row+length>height) end=height;
        for(int r=row; r<end;r++)
            if (image.getRGB(column,r)==color) return true;
        return false;
    }

    boolean oneColorInRow(int row,int column,int length,int color) {
        int end=column+length;
        if (column+length+1>width) end=width-1;
        for(int kol=column; kol<=end;kol++)
            if (image.getRGB(kol,row)!=color) return false;
        return true;
    }

    boolean oneColorInCol(int column,int row,int length,int color) {
        int end=row+length;
        if (row+length+1>height) end=height-1;
        for(int r=row; r<=end;r++)
            if (image.getRGB(column,r)!=color) return false;
        return true;
    }

    Point findRow(int length,int color) {
        Point wynik=null;
        for(int r=0;r<height;r++)
            for(int col=0;col<width-length;col++)
                if (oneColorInRow(r,col,length,color)) return new Point(col,r);
        return wynik;
    }

    Point findCol(int from,int length,int color) {
        Point wynik=null;
        for(int col=from;col<width;col++)
            for(int r=0;r<height-length;r++)
                if (oneColorInCol(col,r,length,color)) return new Point(col,r);
        return wynik;
    }

    Point findCol(int length,int color) {
        return findCol(0,length,color);
    }

    boolean isRectangle(int x0,int y0,int szer,int wys, int color) {
        //czy w punkcie x0,y0 mamy prostokat o szer,wys i w kolorze
        if (image.getRGB(x0,y0)!=color) return false;
        return oneColorInRow(y0,x0,szer,color) &
               oneColorInRow(y0+wys,x0,szer,color) &
               oneColorInCol(x0,y0,wys,color) &
               oneColorInCol(x0+szer,y0,wys,color);
    }

    Point findRectangle(int szer,int wys,int color) {
        Point wynik=null;
        for(int col=0;col<width-szer;col++)
            for(int r=0;r<height-wys;r++)
                if (isRectangle(col,r,szer,wys,color)) return new Point(col,r);
        return wynik;
    }

    Point findWhiteRectangle(int szer,int wys,Rectangle area) {
        return null;
    }

    private ArrayList<Img> getRows0(int biezacy,ArrayList<Img> acc) {
        if (biezacy>=height) return acc;
        if (noColorInRow(biezacy,0,width,Black)) return getRows0(biezacy+1,acc);
        int koniec=biezacy+1;
        while(existsColorInRow(koniec,0,width,Black)) koniec++;
        acc.add(new Img(image.getSubimage(0,biezacy,width,koniec-biezacy)));
        return getRows0(koniec,acc);
    }

    ArrayList<Img> getRows() {
        ArrayList<Img> acc=new ArrayList<Img>();
        return getRows0(0,acc);
    }

    private Img removeNonBlackBorder() {
        int begin=0,end=height-1;
        while(noColorInRow(begin,0,width,Black)) begin++;
        while(noColorInRow(end,0,width,Black)) end--;
        return new Img(image.getSubimage(0,begin,width,1+end-begin));
    }

    Img removeWhiteBorder() {
        int begin=0,end=height-1;
        while(oneColorInRow(begin,0,width,White)) begin++;
        while(oneColorInRow(end,0,width,White)) end--;
        int col0=0,col1=width-1;
        while(oneColorInCol(col0,0,height,White)) col0++;
        while(oneColorInCol(col1,0,height,White)) col1--;
        return new Img(image.getSubimage(col0,begin,1+col1-col0,1+end-begin));
    }

    private ArrayList<Img> getCols0(int biezacy,ArrayList<Img> acc) {
        if (biezacy>=width) return acc;
        if (noColorInCol(biezacy,0,height,Black)) return getCols0(biezacy+1,acc);
        int koniec=biezacy+1;
        while(existsColorInCol(koniec,0,height,Black)) koniec++;
        acc.add(new
                Img(image.getSubimage(biezacy,0,koniec-biezacy,height)).removeNonBlackBorder());
        return getCols0(koniec,acc);
    }

    ArrayList<Img> getCols() {
        ArrayList<Img> acc=new ArrayList<Img>();
        return getCols0(0,acc);
    }

}


