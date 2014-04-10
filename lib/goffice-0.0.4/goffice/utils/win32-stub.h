#ifndef _HTMLHELP_STUB_h_
#define _HTMLHELP_STUB_h_

#include <windows.h>
#include <htmlhelp.h>

HWND HtmlHelp_ (HWND    hwndCaller,
		LPCSTR  pszFile,
		UINT    uCommand,
		DWORD   dwData);

HRESULT FindMimeFromData_(LPBC    pBC,
			  LPCWSTR pwzUrl,
			  LPVOID  pBuffer,
			  DWORD   cbSize,
			  LPCWSTR pwzMimeProposed,
			  DWORD   dwMimeFlags,
			  LPWSTR *ppwzMimeOut,
			  DWORD   dwReserved);

#endif
