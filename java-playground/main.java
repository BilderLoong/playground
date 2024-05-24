import java.util.*;

public class main {
    public static void main(String[] args) {

    Map<String, Integer> map = new HashMap<String,Integer>() {
      {
      System.out.println("init");
      System.out.println(bar);
      put("one", 1);
      put("two", 2);
      put("three", 3);
    }
    };

        System.out.println(map.toString());
        // System.out.println(map.bar);
        System.out.println(new Bar().toString());
    }

    public static void use(String[] args) {
        System.out.println("Hello World!");
    }

}


class Bar extends HashMap<String, Integer>{

    {

        put("a",1);
        System.out.println("runing");
      
    }
  
}
