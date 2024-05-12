import java.util.concurrent.atomic.AtomicLongArray; 

class AcmeSafeState implements State {

    private AtomicLongArray value;

    AcmeSafeState(int length) { value = new AtomicLongArray(length); }

    public int size() { return value.length(); }

    // Synchronized get
    public long[] current() { 
        long[] result;
        synchronized(value) {
            result = new long[value.length()];
            for(int i = 0; i < value.length(); i++)
                result[i] = value.get(i);
        }
        return result;
     }

    public void swap(int i, int j) {
        value.getAndDecrement(i);
        value.getAndIncrement(j);
    }
}