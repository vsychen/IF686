import java.util.ArrayList;

public class obterFicha {
  public static int ficheiro = 0;

  public static void main(String[] args){
    ArrayList<ThreadSample> al = new ArrayList<ThreadSample>();

    for(int i = 0; i < 100; i++){
      ThreadSample ts = new ThreadSample();
      al.add(ts);
      ts.start();
    }
  
    for(ThreadSample t : al){
      t.join();
    }
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  }

  public static synchronized int obterFicha(){
    int retorno = ficheiro;
	ficheiro++;
	return retorno;
  }
}

class ThreadSample extends Thread {
  public void run(){
    int[] fichas = new int[10000];
    int index = 0;
    
    while(index < 10000){
      fichas[index] = obterFicha.obterFicha();
      index++;
    }
  }
}