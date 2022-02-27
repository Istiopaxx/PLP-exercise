using System;
 
public class Test
{
	public static void Main()
	{
		int a = 1;
		int b = 2;
		Action middle = () =>
		{
			int b = a;
			Action inner = () =>
			{
				Console.WriteLine("{0}, {1}", a, b);
			}
			int a = 3;
 
			inner();
			Console.WriteLine("{0}, {1}", a, b);
		}
		middle();
		Console.WriteLine("{0}, {1}", a, b);
	}
}