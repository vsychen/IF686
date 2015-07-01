import java.util.Scanner;

public class Main {
	public static int n;
	public static int i;

	public static void main(String[] args) {
		Scanner s = new Scanner(System.in);
		i = n = s.nextInt();
		s.close();

		(new ThreadPrincipal()).start();
	}
}
