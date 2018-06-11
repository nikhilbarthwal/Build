mport java.util.UUID;
import java.util.Date;
import java.text.SimpleDateFormat;
import java.util.HashMap;
import java.util.Map;

public class guid
{

static final Map<String , String> FLAVORS = new HashMap<String , String>() {{
    put("Up",    "Down");
    put("Charm", "Strange");
    put("Top",   "Bottom");
}};

   public static void main(String[] args)
   {
       String id = UUID.randomUUID().toString();
       String ts = (new SimpleDateFormat("EEEE, dd MMMM yyyy; hh:mm::ss a")).format(new Date());
       System.out.println(id);
       System.out.println(ts);
       System.out.println(FLAVORS.get("fff"));    
   }    
}



