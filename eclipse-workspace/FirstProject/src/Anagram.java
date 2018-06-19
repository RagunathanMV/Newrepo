import java.lang.reflect.Array;
import java.util.*;
public class Anagram
{
	public static void main(String[] args) 
	{
		Scanner s=new Scanner(System.in);
		System.out.println("Enter the strings:");
		String a=s.next();
		String b=s.next();
	//	String a=s1.replaceAll(" "," ");
		//String b=s2.replaceAll(" "," ");
		boolean status=true;
		if(a.length()!=b.length())
		{
			status=false;
		}
		else
		{
			char c1[]=a.toLowerCase().toCharArray();
			char c2[]=b.toLowerCase().toCharArray();
			Arrays.sort(c1);
			Arrays.sort(c2);
			status=Arrays.equals(c1, c2);
			if(status)
			{
				System.out.println("Given worrd is Anagram");
			}
			else
			{
				System.out.println("not an anagram");
			}

		}
			
		
	}

}
