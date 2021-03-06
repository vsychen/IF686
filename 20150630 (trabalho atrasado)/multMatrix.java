import java.util.Scanner;
import java.util.ArrayList;

/* Results in millis
------------------------------------------------------------------------
| N\X |   10   |    100    |     1000     |    10000    |    100000    |
------------------------------------------------------------------------
|  1  |   003  |    003    |     003      |     008     |     086      |
------------------------------------------------------------------------
|  2  |   003  |    003    |     004      |     009     |     086      |
------------------------------------------------------------------------
|  3  |   004  |    004    |     004      |     009     |     086      |
------------------------------------------------------------------------
|  4  |   004  |    004    |     004      |     009     |     086      |
------------------------------------------------------------------------
|  8  |   004  |    005    |     006      |     011     |     087      |
------------------------------------------------------------------------
| 16  |   008  |    008    |     009      |     013     |     088      |
------------------------------------------------------------------------

Unexpected results
// N = 8, X = 100. 1st try -> 2ms
// N = 8, X = 10000. wave between 2~11ms
*/

public class multMatrix {
  public static Matrix m1,m2,result;
  private static final int N = 1;
  private static final int X = 10;
  public static int count;
  
  public static void main (String[] args){
    Scanner in = new Scanner(System.in);
    int y = in.nextInt();
    int z = in.nextInt();
    System.out.println("A primeira matriz tera dimensoes " + X + " por " + y + ". A segunda matriz tera dimensoes " + y + " por " + z + ". ");
    in.close();

    m1 = new Matrix(X, y);
    m2 = new Matrix(y, z);

    for(int i = 0; i < X; i++){
      for(int j = 0; j < y; j++){
        m1.base[i][j].value = i*j;
      }
    }

    for(int i = 0; i < y; i++){
      for(int j = 0; j < z; j++){
        m2.base[i][j].value = (2*i)+(j/2);
      }
    }

    long before = System.currentTimeMillis();

    ArrayList<ThreadSample> al = new ArrayList<ThreadSample>();
    result = new Matrix(X, z);
    count = X*z;

    for(int i = 0; i < N; i++){
      ThreadSample ts = new ThreadSample();
      al.add(ts);
      ts.start();
    }

    try{
      for(ThreadSample t : al){
        t.join();
      }
    } catch (InterruptedException e) {
      e.printStackTrace();
    }

    System.out.println((System.currentTimeMillis() - before) + " ms.");
  }

  public static int calcCoord(Matrix m1, Matrix m2, Point p){
    int retorno = 0;
    int aux = 0;

    while(aux < m1.base[0].length){
      retorno += m1.base[p.x][aux].value * m2.base[aux][p.y].value;
      aux++;
    }

    return retorno;
  }
}

class Node {
  int value;
  
  public Node(int value) {
    this.value = value;
  }
}

class Point {
  int x;
  int y;
  
  public Point(int x, int y){
    this.x = x;
    this.y = y;
  }
}

class Matrix {
  Node[][] base;
  int count;
  
  public Matrix (int n, int m){
    this.base = new Node[n][m];
    this.count = 0;

    for(int i = 0; i < n; i++){
      for(int j = 0; j < m; j++){
        this.base[i][j] = new Node(-1);
      }
    }
  }
  
  public synchronized Point getCoord(){
    Point retorno = null;

    if(count < multMatrix.count){
      retorno = new Point(count/(base[0].length),count%(base[0].length));
      this.count++;
    }

    return retorno;
  }
  
  public void print(){
    for(int i = 0; i < base.length; i++){
      String forPrint = "";

      for(int j = 0; j < base[i].length; j++){
        forPrint = forPrint + base[i][j].value;

        if(j != base[i].length-1){
          forPrint = forPrint + " ";
        }
      }

      System.out.println(forPrint);
    }
  }
}

class ThreadSample extends Thread {
  public void run(){
    Point coord = null;

    while(!interrupted()){
      if(multMatrix.result.count < (multMatrix.count)){
        interrupt();
      }

      coord = multMatrix.result.getCoord();
      multMatrix.result.base[coord.x][coord.y].value = multMatrix.calcCoord(multMatrix.m1, multMatrix.m2, coord);
    }
  }
}