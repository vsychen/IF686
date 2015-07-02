import java.util.ArrayList;
import java.util.concurrent.locks.Lock;
import java.util.concurrent.locks.ReentrantLock;
import java.util.concurrent.locks.Condition;

public class SharedWashroom {
  public static MainThread washroom;
  public static ArrayList<ManThread> mtl;
  public static ArrayList<WomanThread> wtl;
  public static boolean noMoreThreads = false;

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

    noMoreThreads = true;
    System.out.println("Banheiro fechado");
  }
}

class MainThread extends Thread {
  public int manI, womanI, manO, womanO; // how many inside/outside
  public volatile boolean genre;         // false (0) = woman; true (1) = man
  public Lock l;
  private Condition free;

  public MainThread() {
    this.manI = 0;
	this.womanI = 0;
	this.manO = 0;
	this.womanO = 0;
    this.genre = true;
	this.l = new ReentrantLock();
	this.free = l.newCondition();
  }

  public void run() {
    while (manO != 0 || womanO != 0 || !SharedWashroom.noMoreThreads) {
      if (manO == 0) {
        genre = false;
      } else if (womanO == 0) {
        genre = true;
      } else {
        if (manO > 2 * womanO) genre = true;
        else if (womanO > 2 * manO) genre = false;
      }

      l.lock();

      try {
        free.signalAll();
      } finally {
        l.unlock();
      }
    }
  }
  
  public void entrarHomem(int id) throws InterruptedException {
    l.lock();

    try {
      while (!genre || womanI != 0) {
        free.await();
      }

      manO--;
	  manI++;
      System.out.println("Homem " + id + " entrou no banheiro");
    } finally {
      l.unlock();
    }
  }
  
  public void sairHomem(int id) {
    l.lock();

    try {
      manI--;
      System.out.println("Homem " + id + " saiu do banheiro");
    } finally {
      l.unlock();
    }
  }
  
  public void entrarMulher(int id) throws InterruptedException {
    l.lock();

    try {
      while (genre || manI != 0) {
        free.await();
      }

      womanO--;
	  womanI++;
      System.out.println("Mulher " + id + " entrou no banheiro");
    } finally {
      l.unlock();
    }
  }
  
  public void sairMulher(int id) {
    l.lock();

    try {
      womanI--;
      System.out.println("Mulher " + id + " saiu do banheiro");
    } finally {
      l.unlock(); 
    }
  }
}

class ManThread extends Thread {
  public int id;

  public ManThread(int id) {
    this.id = id;
    SharedWashroom.washroom.l.lock();

    try {
      SharedWashroom.washroom.manO++;
    } finally {
      SharedWashroom.washroom.l.unlock();
    }
  }

  public void run() {
    try {
      SharedWashroom.washroom.entrarHomem(id);
    } catch (InterruptedException e) {}

    SharedWashroom.washroom.sairHomem(id);
  }
}

class WomanThread extends Thread {
  public int id;

  public WomanThread(int id) {
    this.id = id;
    SharedWashroom.washroom.l.lock();

    try {
      SharedWashroom.washroom.womanO++;
    } finally {
      SharedWashroom.washroom.l.unlock();
    }
  }	

  public void run() {
    try {
      SharedWashroom.washroom.entrarMulher(id);
    } catch (InterruptedException e) {}

    SharedWashroom.washroom.sairMulher(id);
  }
}