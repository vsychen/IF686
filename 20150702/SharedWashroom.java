import java.util.ArrayList;

public class SharedWashroom {
  public static MainThread washroom;
  public static ArrayList<ManThread> mtl;
  public static ArrayList<WomanThread> wtl;

  public static void main(String[] args) {
    mtl = new ArrayList<ManThread>();
    wtl = new ArrayList<WomanThread>();
    washroom = new MainThread();
    washroom.start();

    for (int i = 0; i < 50; i++) {
      ManThread mt = new ManThread(i);
      mtl.add(mt);
      mt.start();

      WomanThread wt = new WomanThread(i);
      wtl.add(wt);
      wt.start();
    }

    try {
      for (ManThread mt : mtl) {
        mt.join();
      }

      for (WomanThread wt : wtl) {
        wt.join();
      }
    } catch (InterruptedException e) {
      e.printStackTrace();
    }

    washroom.run = false;
    System.out.println("Banheiro fechado");
  }
}

class MainThread extends Thread {
  public volatile int count;
  public boolean genre;
  public boolean run;

  public MainThread() {
    this.count = 0;
    this.genre = true;
    this.run = true;
  }

  public void run() {
    while (run) {
      if (System.currentTimeMillis()%4 == 0) {
        while (count != 0) {}

        genre = !genre;
      }

      notifyAll();
    }
  }
}

class ManThread extends Thread {
  public int id;
  public boolean inside;

  public ManThread(int id) {
    this.id = id;
    this.inside = false;
  }

  public void run() {
    try {
      entrarHomem();
      sairHomem();
    } catch (InterruptedException e) {
      e.printStackTrace();
    }
  }

  public void entrarHomem() throws InterruptedException {
    guard();

    if (!inside) {
      synchronized (SharedWashroom.washroom) {
        if (SharedWashroom.washroom.genre) SharedWashroom.washroom.count++;
        System.out.println("Homem nº " + id + " acabou de entrar no banheiro.");
      }

      inside = true;
    }
  }

  public void sairHomem() {
    if (inside) {
      synchronized (SharedWashroom.washroom) {
        SharedWashroom.washroom.count--;
        System.out.println("Homem nº " + id + " acabou de sair do banheiro.");
      }

      inside = false;
    }
  }

  public void guard() throws InterruptedException {
    synchronized (SharedWashroom.washroom) {
      while (!SharedWashroom.washroom.genre) {
        wait();
      }
    }
  }
}

class WomanThread extends Thread {
  public int id;
  public boolean inside;

  public WomanThread(int id) {
    this.id = id;
    this.inside = false;
  }

  public void run() {
    try {
      entrarMulher();
      sairMulher();
    } catch (InterruptedException e) {
      e.printStackTrace();
    }
  }

  public void entrarMulher() throws InterruptedException {
    guard();

    if (!inside) {
      synchronized (SharedWashroom.washroom) {
        if (!SharedWashroom.washroom.genre) SharedWashroom.washroom.count++;
        System.out.println("Mulher nº " + id + " acabou de entrar no banheiro.");
      }

      inside = true;
    }
  }

  public void sairMulher() {
    if (inside) {
      synchronized (SharedWashroom.washroom) {
        SharedWashroom.washroom.count--;
        System.out.println("Mulher nº " + id + " acabou de sair do banheiro.");
      }

      inside = false;
    }
  }

  public void guard() throws InterruptedException {
    synchronized (SharedWashroom.washroom) {
      while (SharedWashroom.washroom.genre) {
        wait();
      }
    }
  }
}