package guiao9;

import java.io.*;
import java.net.*;
public class Client {
    public static void main(String[] args) {
        try {
            if (args.length < 2)
                System.exit(1);
            String host = args[0];
            int port = Integer.parseInt(args[1]);
            Socket s = new Socket(host, port);
            BufferedReader reader = new BufferedReader(
                    new InputStreamReader(System.in));
            PrintWriter out = new PrintWriter(s.getOutputStream());
            while(true){
                String name = reader.readLine();
                out.println(name);
                out.flush();
            }
        } catch (Exception e) {
            e.printStackTrace();
            System.exit(0);
        }
    }
}