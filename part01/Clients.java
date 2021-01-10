package part01;

public class Clients {
  static List<Integer> doubleAll(List<Integer> xs) {
    return List.map((new Func<Integer, Integer>() {
      public Integer m(Integer x) {
        return x * 2;
      }
    }), xs);
  }

  static int countNs(List<Integer> xs, final int n) {
    return List.length(List.filter((new Pred<Integer>() {
      public boolean m(Integer x) {
        return x == n;
      }
    }), xs));
  }
}
