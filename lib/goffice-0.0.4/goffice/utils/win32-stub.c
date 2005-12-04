#include <goffice/goffice-config.h>
#include "win32-stub.h"
#include <urlmon.h>

HWND HtmlHelp_ (HWND    hwndCaller,
		LPCSTR  pszFile,
		UINT    uCommand,
		DWORD   dwData)
{
	return HtmlHelp (hwndCaller, pszFile, uCommand, dwData);
}

HRESULT FindMimeFromData_(LPBC    pBC,
			  LPCWSTR pwzUrl,
			  LPVOID  pBuffer,
			  DWORD   cbSize,
			  LPCWSTR pwzMimeProposed,
			  DWORD   dwMimeFlags,
			  LPWSTR *ppwzMimeOut,
			  DWORD   dwReserved)
{
	return FindMimeFromData (pBC, pwzUrl, pBuffer, cbSize, pwzMimeProposed,
				 dwMimeFlags, ppwzMimeOut, dwReserved);
}
