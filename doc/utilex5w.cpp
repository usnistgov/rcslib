#include "rcs.hh"
#include "win_prnt.hh"
#include "roots.hh"
#include <windows.h>;

/* A Windows Application which uses compute_roots. */
int PASCAL WinMain( HANDLE hInstance, 
		   HANDLE hPrevInstance, 
		   LPSTR lpszCmdParam,
			int nCmdShow)
{
	double r1, r2;
	MSG msg;

	/* Store all the rcs_print messages in a linked list 
	   that a window can display later. */
	set_rcs_print_destination(RCS_PRINT_TO_LIST);

	/* Create a window to show rcs_print messages, 
	   and display it as an icon to start.
 	It has no parent window. */
	create_rcs_print_window(hInstance, SW_MINIMIZE, NULL); 

	/* Compute the roots of a quadratic that will produce an error. */
	compute_roots(1, 1, 1, r1, r2);

	/* Wait for someone to kill this process. */
	while(GetMessage(&msg, NULL, 0, 0))
	{
	  TranslateMessage(&msg);
	  DispatchMessage(&msg);
	}
	return(msg.wParam);
}
