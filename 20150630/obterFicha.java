import java.util.ArrayList;

/*
Este código faz uso do método sleep() para garantir que uma Thread não consiga acessar o método obterFicha seguidamente.
*/

public class obterFicha {
  public static int ficheiro = 0;

  public static void main(String[] args){
    ArrayList<ThreadSample> al = new ArrayList<ThreadSample>();

    for(int i = 0; i < 100; i++){
      ThreadSample ts = new ThreadSample();
      al.add(ts);
      ts.start();
    }

    try{
      for(ThreadSample t : al){
        t.join();
      }
    } catch(InterruptedException e){
      e.printStackTrace();
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

      try {
        sleep(1);
      } catch(InterruptedException e){
        e.printStackTrace();
      }
    }
  }
}