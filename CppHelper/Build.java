// Main Build class
import java.util.UUID;
import java.util.Date;
import java.text.SimpleDateFormat;

public class Build
{
    public final String Target = "./Target";

    private static String timeStamp;
    private static String id;
    private static boolean init = false;

    public static String TimeStamp()
    {
        if (!init) Init();
        return timeStamp;
    }
    
    public static String Id()
    {
        if (!init) Init();
        return id;
    }
    
    private static void Init()
    {
        timeStamp = (new SimpleDateFormat("EEEE, dd MMMM yyyy; hh:mm::ss a")).format(new Date());
        String id = UUID.randomUUID().toString();
        init = true;
        System.out.println(" * Build Initialized *");
        System.out.println(" Time Stamp: " + timeStamp);
        System.out.println(" Id: " + id);
    }
    
    private static MyList<MyFile> MyBuild()
    {
        // The name of the Generating functions goes here
        return Somewhere();
    }
    public static void Main(String[] args)
    {
        Init();
        
        for(MyFile f: MyBuild())
        {
            f.Generate();
        }
        
        return;
    }
}
