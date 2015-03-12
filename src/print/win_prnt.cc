/* 
The NIST RCS (Real-time Control Systems) 
 library is public domain software, however it is preferred
 that the following disclaimers be attached.

Software Copywrite/Warranty Disclaimer

   This software was developed at the National Institute of Standards and
Technology by employees of the Federal Government in the course of their
official duties. Pursuant to title 17 Section 105 of the United States
Code this software is not subject to copyright protection and is in the
public domain. NIST Real-Time Control System software is an experimental
system. NIST assumes no responsibility whatsoever for its use by other
parties, and makes no guarantees, expressed or implied, about its
quality, reliability, or any other characteristic. We would appreciate
acknowledgement if the software is used. This software can be
redistributed and/or modified freely provided that any derivative works
bear some notice that they are derived from it, and any modified
versions bear some notice that they have been modified.



*/ 

/* This line prevents findsource from adding this file to the Makefile */
#define NOT_A_DEFAULT_RCSLIB_SOURCE 1

#if defined(WIN32) || defined(WIN16) || defined(MS_WINDOWS_API) || defined(_WINDOWS) || defined(_Windows) || defined(_MSC_VER) || defined(WINDOWS)

/*********************************************************************
* File: win_prnt.cc
* Purpose: To make RCS applications that use commands like printf more
* easily ported to Windows and to give windows users the debug and error
* information normally sent to the stdout or stderr.
* The printf calls in the RCS library have been replaced with rcs_print
* and it's cousins in rcs_prnt.cc. For programs that can be run from a
* dos or unix command shell the output is directed to stderr or stdout so
* they work as before. But under Windows the output is directed to a linked
* list called the rcs_print_list. The functions in this file create, update,
* and manipulate a window for viewing that linked list.
***********************************************************************/
#include <windows.h>
#pragma hdrstop

#include "rcs_defs.hh"		/* RCS_CALLBACK_FUNC, _Windows */

#ifdef _Windows

#include <windows.h>		/* almost everything */
#include <stdlib.h>		// abs()
#include <string.h>		/* strlen() */
#include "win_prnt.hh"
#include "rcs_prnt.hh"		/* get_rcs_print_list(), */
				/* set_rcs_print_notify(), */
				/* get_rcs_lines_table() */


void (*callback_on_rcs_print_window_close) (void) = NULL;

#if _MSC_VER >= 1200
extern long RCS_FAR RCS_WINPROC_TYPE rcs_window_proc (HWND _hwnd,
						      UINT _message,
						      UINT _wParam,
						      LONG _lParam);
#else
extern long RCS_FAR RCS_WINPROC_TYPE rcs_window_proc (void RCS_FAR * _hwnd,
						      unsinged int _message,
						      unsigned int _wParam,
						      LONG _lParam);
#endif


void RCS_FAR RCS_PASCAL update_rcs_print_window ();

/* Private Global Data */
WNDCLASS wndclass;
void RCS_FAR *hwnd = NULL;
void RCS_FAR *hwndParent = NULL;
void RCS_FAR *hInstance = NULL;
short char_width = 10;
short char_height = 10;
int x = 0;
int y = 0;
int max_line_width = 0;
int client_height = 0;
int client_width = 0;
int number_of_lines_in_memory = 0;
int number_of_lines_on_screen = 0;
int current_line = 0;
int current_collumn = 0;
int number_of_collumns_on_screen = 0;
RECT client_rect;

/********************************************************************
* Function: update_rcs_print_window()
* Purpose: Forces the ouput window (if it has been created) to update
* its display. Use set_rcs_print_notify so that it will be called
* whenever there`s an rcs_print.
********************************************************************/
void RCS_FAR RCS_PASCAL
update_rcs_print_window ()
{
  RECT invalid_rect;
  update_lines_table ();
  number_of_lines_in_memory = get_rcs_print_list_size ();
  if (hwnd != NULL)		/* Check to see if window has been created. */
    {

      /* If the display region hasn't been filled yet we don't have to */
      /* scroll the screen. */
      if (number_of_lines_in_memory - current_line <
	  number_of_lines_on_screen)
	{
	  invalid_rect.top = current_line * char_height;
	  invalid_rect.left = 0;
	  invalid_rect.bottom = number_of_lines_in_memory * char_height;
	  invalid_rect.right = client_width;
	  InvalidateRect ((HWND) hwnd, &invalid_rect, FALSE);
	}
      else
	{
	  current_line =
	    number_of_lines_in_memory - number_of_lines_on_screen;
	  SetScrollRange ((HWND) hwnd, SB_VERT, 0, current_line, FALSE);
	  SetScrollPos ((HWND) hwnd, SB_VERT, current_line, TRUE);
	  invalid_rect.top = 0;
	  invalid_rect.left = 0;
	  invalid_rect.bottom = client_height;
	  invalid_rect.right = client_width;
	  InvalidateRect ((HWND) hwnd, &invalid_rect, TRUE);
	}
    }
}

void RCS_FAR *RCS_EXPORT
create_default_rcs_print_window ()
{
  return create_rcs_print_window (NULL, SW_SHOW, NULL);
}

void RCS_FAR *RCS_EXPORT
create_rcs_print_window (void RCS_FAR * _hInstance, int _nCmdShow,
			 void RCS_FAR * _hwndParent)
{
  if (hwnd == NULL)
    {
      hInstance = _hInstance;
      hwndParent = _hwndParent;

      wndclass.style = CS_HREDRAW | CS_VREDRAW;
      wndclass.lpfnWndProc = rcs_window_proc;
      wndclass.cbClsExtra = 0;
      wndclass.cbWndExtra = 0;
      wndclass.hInstance = (HINSTANCE) hInstance;
      wndclass.hIcon = LoadIcon (NULL, IDI_APPLICATION);
      wndclass.hCursor = LoadCursor (NULL, IDC_ARROW);
      wndclass.hbrBackground = (HBRUSH) GetStockObject (WHITE_BRUSH);
      wndclass.lpszMenuName = NULL;
      wndclass.lpszClassName = TEXT ("RCS_OUT");

      if (!RegisterClass (&wndclass))
	{
	  return NULL;
	}

      hwnd = CreateWindow (TEXT ("RCS_OUT"),
			   TEXT ("RCS_OUT"),
			   WS_OVERLAPPED | WS_CAPTION | WS_SYSMENU |
			   WS_VSCROLL | WS_HSCROLL, CW_USEDEFAULT,
			   CW_USEDEFAULT, CW_USEDEFAULT, CW_USEDEFAULT,
			   (HWND) hwndParent, NULL, (HINSTANCE) hInstance,
			   NULL);

      if (NULL == hwnd)
	{
	  return NULL;
	}

      ShowWindow ((HWND) hwnd, _nCmdShow);
      UpdateWindow ((HWND) hwnd);

      set_rcs_print_notify ((RCS_PRINT_NOTIFY_FUNC_PTR)
			    update_rcs_print_window);
    }
  return (hwnd);
}

void RCS_EXPORT
remove_rcs_print_window ()
{
  if (NULL != hwnd)
    {
      PostMessage ((HWND) hwnd, WM_DESTROY, 0, 0);
    }
}

#if _MSC_VER >= 1200
extern long RCS_FAR RCS_WINPROC_TYPE
rcs_window_proc (HWND _hwnd, UINT _message, UINT _wParam, LONG _lParam)
#else
extern long RCS_FAR RCS_WINPROC_TYPE
rcs_window_proc (void RCS_FAR * _hwnd,
		 UINT _message, UINT _wParam, LONG _lParam)
#endif
{
  HDC hdc;
  TEXTMETRIC tm;
  RECT invalid_rect;

  switch (_message)
    {
    case WM_CREATE:
      hdc = GetDC (_hwnd);

      if (NULL == hdc)
	{
	  return (-1);
	}
      GetTextMetrics (hdc, &tm);
      char_width = (short) tm.tmAveCharWidth;
      char_height = (short) (tm.tmHeight + tm.tmExternalLeading);
      GetClientRect (_hwnd, &client_rect);
      client_width = abs (client_rect.right - client_rect.left);
      if (client_width < 10)
	{
	  client_width = 10;
	}
      client_height = abs (client_rect.top - client_rect.bottom);
      if (client_height < 10)
	{
	  client_height = 10;
	}
      number_of_lines_on_screen = client_height / char_height;
      if (number_of_lines_on_screen < 5)
	{
	  number_of_lines_on_screen = 5;
	}
      if (number_of_lines_on_screen > 250)
	{
	  number_of_lines_on_screen = 250;
	}
      ReleaseDC (_hwnd, hdc);
      number_of_lines_in_memory = 0;
      max_line_width = 0;
      current_line = 0;
      current_collumn = 0;
      return 0;

    case WM_SIZE:
      client_height = HIWORD (_lParam);
      client_width = LOWORD (_lParam);
      number_of_lines_on_screen = client_height / char_height;
      number_of_collumns_on_screen = client_width / char_width;

      if (number_of_lines_in_memory > number_of_lines_on_screen)
	{
	  SetScrollRange (_hwnd, SB_VERT, 0,
			  number_of_lines_in_memory -
			  number_of_lines_on_screen, FALSE);
	  SetScrollPos (_hwnd, SB_VERT, current_line, TRUE);
	}
      else
	{
	  SetScrollRange (_hwnd, SB_VERT, 0, 0, FALSE);
	  current_line = 0;
	  SetScrollPos (_hwnd, SB_VERT, 0, TRUE);
	}

      if (max_line_width > number_of_collumns_on_screen)
	{
	  SetScrollRange (_hwnd, SB_HORZ, 0,
			  max_line_width - number_of_collumns_on_screen,
			  FALSE);
	  SetScrollPos (_hwnd, SB_HORZ, current_collumn, TRUE);
	}
      else
	{
	  SetScrollRange (_hwnd, SB_HORZ, 0, 0, FALSE);
	  current_collumn = 0;
	  SetScrollPos (_hwnd, SB_HORZ, 0, TRUE);
	}
      invalid_rect.top = 0;
      invalid_rect.left = 0;
      invalid_rect.bottom = client_height;
      invalid_rect.right = client_width;
      InvalidateRect (_hwnd, &invalid_rect, TRUE);
      return 0;

    case WM_VSCROLL:
      switch (_wParam)
	{
	case SB_TOP:
	  current_line = 0;
	  break;

	case SB_BOTTOM:
	  current_line =
	    number_of_lines_in_memory - number_of_lines_on_screen;
	  break;

	case SB_LINEUP:
	  if (current_line > 0)
	    {
	      current_line--;
	    }
	  break;

	case SB_LINEDOWN:
	  if (current_line <
	      number_of_lines_in_memory - number_of_lines_on_screen)
	    {
	      current_line++;
	    }
	  break;

	case SB_PAGEUP:
	  if (current_line > number_of_lines_on_screen)
	    {
	      current_line -= number_of_lines_on_screen;
	    }
	  else
	    {
	      current_line = 0;
	    }
	  break;

	case SB_PAGEDOWN:
	  if (current_line <
	      number_of_lines_in_memory - 2 * number_of_lines_on_screen)
	    {
	      current_line += number_of_lines_on_screen;
	    }
	  else
	    {
	      current_line =
		number_of_lines_in_memory - number_of_lines_on_screen;
	    }
	  break;

	case SB_THUMBTRACK:
	  int track_pos;
	  track_pos = LOWORD (_lParam);
	  if ((track_pos > 0) &&
	      (track_pos <
	       number_of_lines_in_memory - number_of_lines_on_screen))
	    {
	      current_line = track_pos;
	    }
	  break;

	default:
	  break;
	}
      SetScrollPos (_hwnd, SB_VERT, current_line, TRUE);
      invalid_rect.top = 0;
      invalid_rect.left = 0;
      invalid_rect.bottom = client_height;
      invalid_rect.right = client_width;
      InvalidateRect (_hwnd, &invalid_rect, TRUE);
      return 0;

    case WM_HSCROLL:
      switch (_wParam)
	{
	case SB_TOP:
	  current_collumn = 0;
	  break;

	case SB_BOTTOM:
	  current_collumn = max_line_width - number_of_collumns_on_screen;
	  break;

	case SB_LINEUP:
	  if (current_collumn > 0)
	    {
	      current_collumn--;
	    }
	  break;

	case SB_LINEDOWN:
	  if (current_collumn < max_line_width - number_of_collumns_on_screen)
	    {
	      current_collumn++;
	    }
	  break;

	case SB_PAGEUP:
	  if (current_collumn > number_of_collumns_on_screen)
	    {
	      current_collumn -= number_of_collumns_on_screen;
	    }
	  else
	    {
	      current_collumn = 0;
	    }
	  break;

	case SB_PAGEDOWN:
	  if (current_collumn <
	      max_line_width - 2 * number_of_collumns_on_screen)
	    {
	      current_collumn += number_of_collumns_on_screen;
	    }
	  else
	    {
	      current_collumn = max_line_width - number_of_collumns_on_screen;
	    }
	  break;

	case SB_THUMBTRACK:
	  int thumb_pos;
	  thumb_pos = LOWORD (_lParam);
	  if ((thumb_pos > 0) &&
	      (thumb_pos < max_line_width - number_of_collumns_on_screen))
	    {
	      current_collumn = thumb_pos;
	    }
	  break;

	default:
	  break;
	}
      SetScrollPos (_hwnd, SB_HORZ, current_collumn, TRUE);
      invalid_rect.top = 0;
      invalid_rect.left = 0;
      invalid_rect.bottom = client_height;
      invalid_rect.right = client_width;
      InvalidateRect (_hwnd, &invalid_rect, TRUE);
      return 0;


    case WM_PAINT:
      PAINTSTRUCT ps;
      int line_width;
      hdc = BeginPaint (_hwnd, &ps);
      x = (10 + char_width) - current_collumn * char_width;
      int max_line_width_changed;
      max_line_width_changed = 0;
      if (NULL != get_rcs_print_list ())
	{
	  if (NULL != get_rcs_lines_table ())
	    {
	      for (int i = current_line; (i < number_of_lines_in_memory) &&
		   (i < current_line + number_of_lines_on_screen); i++)
		{
		  y = (i - current_line) * char_height;
		  char RCS_FAR *cleaned_line;
		  cleaned_line = strip_control_characters (NULL,
							   get_rcs_lines_table
							   ()[i]);
		  if (NULL != cleaned_line)
		    {
		      line_width = strlen (cleaned_line);
		      if (line_width > max_line_width)
			{
			  max_line_width = line_width;
			  max_line_width_changed = 1;
			}
		      line_width =
			line_width <
			(client_width +
			 current_collumn) ? line_width : client_width +
			current_collumn;
		      int temp_retval = 0;
		      temp_retval =
			TextOut (hdc, x, y, cleaned_line, line_width);
		    }
		}
	    }
	}
      EndPaint (_hwnd, &ps);
      if (max_line_width_changed)
	{
	  SetScrollRange (_hwnd, SB_HORZ, 0,
			  max_line_width - number_of_collumns_on_screen,
			  TRUE);
	}
      return (0);

    case WM_DESTROY:
      if (NULL == hwndParent)
	{
	  PostQuitMessage (0);
	}
      if (NULL != callback_on_rcs_print_window_close)
	{
	  (*callback_on_rcs_print_window_close) ();
	}
      hwnd = NULL;
      return (0);
    }

  return (DefWindowProc (_hwnd, _message, _wParam, _lParam));
}


unsigned long RCS_EXPORT
run_rcs_print_window (void *param)
{
  MSG msg;
  void RCS_FAR *hwnd;
  hwnd = create_default_rcs_print_window ();

  while (GetMessage (&msg, (HWND) hwnd, 0, 0) && NULL != hwnd)
    {
      TranslateMessage (&msg);
      DispatchMessage (&msg);
    }
  if (NULL != callback_on_rcs_print_window_close)
    {
      (*callback_on_rcs_print_window_close) ();
    }
  ExitThread (0);
  return 0;
}

DWORD rcs_print_window_thread_id = 0;
HANDLE rcs_print_window_thread_handle = NULL;
void RCS_EXPORT
start_rcs_print_window (void *param)
{
  set_rcs_print_notify ((RCS_PRINT_NOTIFY_FUNC_PTR) update_rcs_print_window);
  rcs_print_window_thread_handle = CreateThread (NULL,	//LPSECURITY_ATTRIBUTES lpThreadAttributes, pointer to thread security attributes
						 16384,	// DWORD dwStackSize, initial thread stack size, in bytes
						 (LPTHREAD_START_ROUTINE) run_rcs_print_window,	// LPTHREAD_START_ROUTINE lpStartAddress,  pointer to thread function
						 param,	// LPVOID lpParameter, argument for new thread
						 0,	//DWORD dwCreationFlags,  creation flags
						 &rcs_print_window_thread_id	// LPDWORD lpThreadId      // pointer to returned thread identifier
    );
  int tries = 0;
  while (NULL == hwnd && tries < 30)
    {
      Sleep (30);
      tries++;
    }
  return;
}



#endif

// end of #if defined(WIN32) || defined(WIN16) || defined(MS_WINDOWS_API) || defined(_WINDOWS) || defined(_Windows) || defined(_MSC_VER) || defined(WINDOWS)
#else
#include "rcs_empty_source"
#endif
