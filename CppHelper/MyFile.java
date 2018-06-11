public class MyFile
{
    private MyList<String> location;
    private MyList<MyLine> myLines;
    
    public MyFile(MyList<String> Location, MyList<MyLine> Lines)
    {
        location = new MyList<String>(Location);
        myLines = new MyList<MyLine>(Lines);
        location.Lock();
        myLines.Lock();
    }

    void Generate()
    {
        return;
    }
}
