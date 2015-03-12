import java.awt.*;

// Button to reset time.
public class CountButton extends Button
{
	int count = 0;
	public CountButton(String str)
	{
		setLabel(str);
	}
	public boolean action(Event evt, Object what)
	{
	   count++;
	   return true;
	}
}
