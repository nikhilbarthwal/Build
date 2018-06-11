import java.util.Iterator;
import java.util.ArrayList;

public class MyList<T> implements java.lang.Iterable<T>, Iterator<T>
{
    private ArrayList<T> data;
    private boolean lock = false;
    private int pos = 0;

    public MyList()
    {
        return;
    }
    
    public MyList(MyList<T> x)
    {
        for(T k:x) data.add(k);
        return;
    }
    
    public MyList(MyList<T> x, MyList<T> y)
    {
        for(T k:x) data.add(k);
        for(T k:x) data.add(k);

        return;
    }
    
    void Add(T x) throws Exception
    {
        if (lock)
            throw new Exception("Wrong!");
        else
            data.add(x);
    }

    void Lock()
    {
        lock = true;
    };

    public boolean hasNext()
    {
        lock = true;
        return (pos < data.size()); 
    }

    public T next()
    {
        lock = true;
        T x = data.get(pos);
        pos++;
        return x;
    }

    public void remove()
    {
        throw new UnsupportedOperationException();
    }

    // @Override
    public Iterator<T> iterator()
    {
        this.lock = true;
        this.pos = 0;
        return this;
    }
}
