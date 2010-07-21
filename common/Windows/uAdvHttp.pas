//
// TTTTTTTTTTTT  HH                            VV          VV  NN     NN  CCCCCCCCCC
//      TT       HH           II                VV        VV   NNN    NN  CC
//      TT       HH                              VV      VV    NNNN   NN  CC
//      TT       HHHHHHHHHHH  II   NNNNNNNNN      VV    VV     NN NN  NN  CC
//      TT       HH       HH  II  NN       NN      VV  VV      NN  NN NN  CC
//      TT       HH       HH  II  NN       NN       VVVV       NN   NNNN  CC
//      TT       HH       HH  II  NN       NN        VV        NN    NNN  CCCCCCCCCC
//
// Copyright 2010 Cybele Software, Inc.
//
//
//
// This file is part of ThinVNC.
//
// ThinVNC is free software: you can redistribute it and/or modify
//     it under the terms of the GNU General Public License as published by
//     the Free Software Foundation, either version 3 of the License, or
//     (at your option) any later version.
//
//     ThinVNC is distributed in the hope that it will be useful,
//     but WITHOUT ANY WARRANTY; without even the implied warranty of
//     MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
//     GNU General Public License for more details.
//
//     You should have received a copy of the GNU General Public License
//     along with ThinVNC. If not, see <http://www.gnu.org/licenses/>
//
// For additional information, please refer to our Licensing FAQ or contact us via e-mail.
//
// See also:
// http://en.wikipedia.org/wiki/GPL
//

// History:
// 30-01-99. Release Version 1.0.
// 03-02-99. Fixed authentication error when setting UserName and Password before
//           execute the Start method.
// 03-02-99. Fixed event fire OnRequestComplete before process authentication.
// 20-02-99. Fixed parsing of response headers.
// 13-03-99. Added StartSync method.
// 13-03-99. Added ConnectTimeout property.
// 16-03-99. Fixed memory leak at GetBody.
// 16-03-99. Fixed resource leak at Cleanup.
// 18-03-99. Added SynchronizeEvent property.
// 18-03-99. Release Version 1.1
// 30-06-99. Fixed Extrainfo property not passed to HttpRequest.
// 20-07-99. Fixed not syncronized access to FStatusInformation in RequestComplete method
// 20-07-99. Release Version 1.2
// 26-08-99. Fixed CloseHandle(hThread) in Cleanup.
// 16-12-99. Changed Start and StartSync Method. Added true syncronous processing.
// 16-01-00. Release Version 1.3.
// 28-01-00. Fixed procedure SetUserAgent.
// 06-06-00. Fixed Setting Proxy property in SetProxy method. Thanks Jon Bondy!
// 25-08-00. Fix in auth problem in RequestComplete procedure. Thaks Ward van Wanrooij!
// 03-10-00. Fix in TimeOut parameter for StartSync method. Reported by Linn Hart.
// 15-03-03. Fix on timeout parameter for Stat method
// 16-03-03. Added ReceiveTimeout property.
// 16-03-03. Added more control on exceptions thru raiseexception property
{:Advanced Http unit}
unit uAdvHttp;

interface

uses
  Windows, Messages, SysUtils, Classes, Controls,WinInet,forms,
  ThinVnc.Log;

{$WARNINGS OFF}
type
{:TInternetFlags is a wrapper of the flags used in HttpOpenRequest wininet function.
  <br><b>ifCacheIfNetFail</b><br>
  Return the resource from the cache if the network request for the resource fails due to an ERROR_INTERNET_CONNECTION_RESET or ERROR_INTERNET_CANNOT_CONNECT.
  <br><b>ifDontCache</b><br>
  Does not add the returned entity to the cache. Identical to the preferred value, ifNoCacheWrite.
  <br><b>ifHyperlink</b><br>
  Forces a reload if there was no Expires time and no Last-Modified time returned from the server when determining whether to reload the item from the network.
  <br><b>ifIgnoreCertCnInvalid</b><br>
  Disables Win32 Internet function checking of SSL/PCT-based certificates that are returned from the server against the host name given in the request. Win32 Internet functions use a simple check against certificates by comparing for matching host names and simple wildcarding rules.
  <br><b>ifIgnoreCertDateInvalid</b><br>
  Disables Win32 Internet function checking of SSL/PCT-based certificates for proper validity dates.
  <br><b>ifIgnoreredirectToHttp</b><br>
  Disables the ability of the Win32 Internet functions to detect this special type of redirect. When this flag is used, Win32 Internet functions transparently allow redirects from HTTPS to HTTP URLs.
  <br><b>ifIgnoreredirectToHttps</b><br>
  Disables the ability of the Win32 Internet functions to detect this special type of redirect. When this flag is used, Win32 Internet functions transparently allow redirects from HTTP to HTTPS URLs.
  <br><b>ifKeepConnection</b><br>
  Uses keep-alive semantics, if available, for the connection. This flag is required for Microsoft Network (MSN), NT LAN Manager (NTLM), and other types of authentication.
  <br><b>ifNeedFile</b><br>
  Causes a temporary file to be created if the file cannot be cached.
  <br><b>ifNoAuth</b><br>
  Does not attempt authentication automatically.
  <br><b>ifNoAutoRedirect</b><br>
  Does not automatically handle redirection in HttpSendRequest.
  <br><b>ifNoCacheWrite</b><br>
  Does not add the returned entity to the cache.
  <br><b>ifNoCookies</b><br>
  Does not automatically add cookie headers to requests, and does not automatically add returned cookies to the cookie database.
  <br><b>ifNoUi</b><br>
  Disables the cookie dialog box.
  <br><b>ifPragmaNoCache</b><br>
  Forces the request to be resolved by the origin server, even if a cached copy exists on the proxy.
  <br><b>ifReload</b><br>
  Forces a download of the requested file, object, or directory listing from the origin server, not from the cache.
  <br><b>ifResynchronize</b><br>
  Reloads HTTP resources if the resource has been modified since the last time it was downloaded.
  <br><b>ifSecure</b><br>
  Uses SSL/PCT transaction semantics.}
  TInternetFlags = set of
    (ifCacheIfNetFail,ifDontCache,ifHyperLink,
    ifIgnoreCertCnInvalid,ifIgnoreCertDateInvalid,
    ifIgnoreRedirectToHttp,ifIgnoreRedirectToHttps,
    ifKeepConnection,ifNeedFile,ifNoAuth,
    ifNoAutoRedirect,ifNoCacheWrite,ifNoCookies,
    ifNoUi,ifPragmaNoCache,ifReload,ifResynchronize);

{:TSynchronizeMode specifies the synchronize way used in StartSync method.
  <br><b>smWinInet</b><br>
  Uses WinInet's INTERNET_FLAG_ASYNC flag to synchronize the response.
  <br><b>smWaitFor</b><br>
  Uses an event object to synchronize and wait the response. }
  TSynchronizeMode = (smWinInet, smWaitFor);

  EInetError = Class(Exception);
{:This component implements a complete encapsulation of http protocol that provides
the Microsoft Internet Explorer Wininet api.
Implements asynchronous http transactions, allowing multiple/concurrent/nonblocking
http requests.
}
  TAdvHttp = class(TComponent)
  private
    { Private declarations }
    FUserAgent   : string;
    hOpen,hConnect,hResource : HINTERNET;
    FSSL: boolean;
    FProxy: string;
    FRaiseExceptions: boolean;
    FURL: string;
    FStatusInformation : Pointer;
    FStatusInformationLength : DWORD;
    FLastError : DWORD;
    FResponseHeaders : TStringList;
    FRequestHeaders : TStringList;
    FOnSendingRequest: TNotifyEvent;
    FOnRequestSent: TNotifyEvent;
    FOnClosingConnection: TNotifyEvent;
    FOnNameResolved: TNotifyEvent;
    FOnConnectedToServer: TNotifyEvent;
    FOnRedirect: TNotifyEvent;
    FOnIntermediateResponse: TNotifyEvent;
    FOnConnectingToServer: TNotifyEvent;
    FOnResponseReceived: TNotifyEvent;
    FOnRequestComplete: TNotifyEvent;
    FOnStateChange: TNotifyEvent;
    FOnConnectionClosed: TNotifyEvent;
    FOnReceivingResponse: TNotifyEvent;
    FOnResolvingName: TNotifyEvent;
    FIscCallback : TFNInternetStatusCallback;
    FOnDataAvailable: TNotifyEvent;
    FResponseStream : TMemoryStream;
    FRequestStream : TMemoryStream;
    FLastStreamPointer : Integer;
    FSynchronizeException: TObject;
    FFunc: TNotifyEvent;
    FPassword: string;
    FUserName: string;
    FMethod: string;
    FHostName: string;
    FPort: Integer;
    FOnError: TNotifyEvent;
    FURLPath: string;
    FExtraInfo: string;
    FThreadId : DWORD;
    hThread : THandle;
    FProxyUserName: string;
    FProxyPassword: string;
    FOnNeedAuth: TNotifyEvent;
    FOnNeedProxyAuth: TNotifyEvent;
    FDefaultUi: Boolean;
    FCanceled : Boolean;
    FHTTPStatus : Integer;
    FHostAddress: string;
    FBytesSent : Integer;
    FBytesReceived : Integer;
    FTotalBytesReceived : Integer;
    FBytesAvailable : Integer;
    FTotalBytesAvailable : Integer;
    FRedirectedUrl : string;
    FInternetFlags: TInternetFlags;
    FIFlags : DWORD;
    FAuthRetries: Integer;
    FAuthCount : Integer;
    FAcceptTypes: TStringList;
    FFreeOnTerminate: Boolean;
    hFinish : THandle;
    FConnectTimeout: integer;
    FReceiveTimeout: integer;
    FSynchronizeEvents: Boolean;
    FBusy : Boolean;
    FSync : DWORD;
    FSynchronizeMode: TSynchronizeMode;
    procedure IstCallback(hInt:HINTERNET;dwContext,dwInternetStatus:DWORD;
                         lpvStatusInformation:Pointer;
                         dwStatusInformationLength:DWORD);
    procedure SetUserAgent(const Value: string);
    function  Initialize:Boolean;
    function  UnInitialize:Boolean;
    procedure SetProxy(const Value: string);
    procedure SetSSL(const Value: boolean);
    procedure SetRaiseExceptions(const Value: boolean);
    procedure SetURL(const Value: string);
    procedure RequestComplete;
    procedure INetCheck(b: LongBool);
    procedure SetOnClosingConnection(const Value: TNotifyEvent);
    procedure SetOnConnectedToServer(const Value: TNotifyEvent);
    procedure SetOnConnectingToServer(const Value: TNotifyEvent);
    procedure SetOnConnectionClosed(const Value: TNotifyEvent);
    procedure SetOnIntermediateResponse(const Value: TNotifyEvent);
    procedure SetOnNameResolved(const Value: TNotifyEvent);
    procedure SetOnReceivingResponse(const Value: TNotifyEvent);
    procedure SetOnRedirect(const Value: TNotifyEvent);
    procedure SetOnRequestComplete(const Value: TNotifyEvent);
    procedure SetOnRequestSent(const Value: TNotifyEvent);
    procedure SetOnResolvingName(const Value: TNotifyEvent);
    procedure SetOnResponseReceived(const Value: TNotifyEvent);
    procedure SetOnSendingRequest(const Value: TNotifyEvent);
    procedure SetOnStateChange(const Value: TNotifyEvent);
    procedure GetBody;
    procedure GetHeaders;
    procedure SetOnDataAvailable(const Value: TNotifyEvent);
    function  GetText : AnsiString;
    procedure SetText( text :  string);
    function  NeedAuth: boolean;
    procedure SetMethod(const Value: string);
    procedure SetPassword(const Value: string);
    procedure SetUserName(const Value: string);
    procedure SetHostName(const Value: string);
    procedure SetPort(const Value: Integer);
    procedure SetLastError(const Value: DWORD);
    procedure SetOnError(const Value: TNotifyEvent);
    function  CrackUrl: boolean;
    procedure SetURLPath(const Value: string);
    procedure SetExtraInfo(const Value: string);
    function  CombineUrl: boolean;
    procedure SetProxyPassword(const Value: string);
    procedure SetProxyUserName(const Value: string);
    procedure SetOnNeedAuth(const Value: TNotifyEvent);
    procedure SetOnNeedProxyAuth(const Value: TNotifyEvent);
    procedure SetDefaultUi(const Value: Boolean);
    procedure Cleanup;
    procedure SendRequest;
    function  GetSuccess:Boolean;
    procedure SetInternetFlags(const Value: TInternetFlags);
    procedure SetAuthRetries(const Value: Integer);
    procedure SetAcceptTypes(const Value: TStringList);
    procedure SetRequestHeaders(const Value: TStringList);
    procedure SetFreeOnTerminate(const Value: Boolean);
    procedure SetConnectTimeout(const Value: integer);
    procedure SetSynchronizeEvents(const Value: Boolean);
    procedure SetBusy(const Value: Boolean);
    procedure SetSynchronizeMode(const Value: TSynchronizeMode);
  protected
    { Protected declarations }
    procedure Synchronize(Method: TNotifyEvent);
    procedure PostFree;
  public
{:Creates an instance of TAdvHttp.
<br>Call Create to create an instance of TAdvHttp at runtime.
<br>After calling the inherited constructor, Create initializes the following properties:
<br>* UserAgent to 'TeamSoft WinInet Component'
<br>* RaiseExceptions to True
<br>* SSL to False
<br>* Method to 'GET'
<br>* HostName to 'localhost'
<br>* Port:=80;
<br>* DefaultUi to True
<br>* FreeOnTerminate to False
}
    constructor create(aowner: TComponent);override;
{:Disposes of the component and its owned components.}
    destructor  destroy;override;
{:Start the http request.
 @result true if request was succefull started.}
    function  Start(Sync:Boolean=False;TimeOut:Integer=0;RcvTimeOut:Integer=0): boolean;
{:Start the http request and wait a timeout value for the response.
 @result true if request was succefully completed.}
    function  StartSync(Timeout:Integer=0;RcvTimeOut:Integer=0): boolean;
{:Cancel the http request.}
    procedure Stop;
{:Allows to get an error description.
 @param dwError Integer containing the error code, normally the LastError property.
 @result String with the error description.
 @seeAlso LastError}
    function  FormatMessage(error: DWORD): string;
{:Returns the last wininet or win32 error code.
@seeAlso FormatMessage}
    property  LastError : DWORD read FLastError;
{:Returns a string list with all returned HTTP headers.}
    property  ResponseHeaders : TStringList read FResponseHeaders;
{:Memory stream containing the complete response data.}
    property  ResponseStream:TMemoryStream read FResponseStream;
{:Input memory stream. Normally contains the form data to be submited.}
    property  RequestStream:TMemoryStream read FRequestStream;
{:HTTP status code response to the last request.}
    property  HTTPStatus:Integer read FHTTPStatus;
{:Server Host Address. Available on OnNameResolved event.
@seeAlso OnNameResolved}
    property  HostAddress : string read FHostAddress;
{:Number of bytes sent on http request. Available on OnRequestsetn event.
@seeAlso OnRequestSent}
    property  BytesSent : Integer read FBytesSent;
{:Number of bytes received in last response. Available on OnResponseReceived event
@seeAlso OnResponseReceived
@seeAlso TotalBytesReceived}
    property  BytesReceived : Integer read FBytesReceived;
{:Total number of bytes received in response. Available on OnResponseReceived event
@seeAlso OnResponseReceived
@seeAlso BytesReceived}
    property  TotalBytesReceived : Integer read FTotalBytesReceived;
{:Number of bytes available from last read. Available on OnDataAvailable event
@seeAlso OnDataAvailable
@seeAlso TotalBytesAvailable}
    property  BytesAvailable : Integer read FBytesAvailable;
{:Total number of bytes available from reading the complete response. Available on OnDataAvailable event
@seeAlso OnDataAvailable
@seeAlso BytesAvailable}
    property  TotalBytesAvailable : Integer read FTotalBytesAvailable;
{:URL to be redirected. Available on OnRedirect event.
@seeAlso OnRedirect}
    property  RedirectedUrl : string read FRedirectedUrl;
{:Success is True on Http Status 200.
@seeAlso HttpStatus}
    property  Success:Boolean read GetSuccess;
{:Determines whether the AdvHttp object is automatically destroyed when the http
transaction terminates. Defaut False.}
    property  FreeOnTerminate:Boolean read FFreeOnTerminate write SetFreeOnTerminate;
{:Synchronize the events trought a window messages. Default True.}
    property  SynchronizeEvents : Boolean read FSynchronizeEvents write SetSynchronizeEvents;
    property  Busy:Boolean read FBusy write SetBusy;
    property  SynchronizeMode: TSynchronizeMode read FSynchronizeMode write SetSynchronizeMode;
  published
    { Published declarations }
{:Returns or sets the time-out value, in milliseconds,
 to use for Internet connection requests.
 If a connection request takes longer than this time-out value,
 the request is canceled. Default 30000 milliseconds}
    property  ConnectTimeout:integer read FConnectTimeout write SetConnectTimeout default 0;
    property  ReceiveTimeout:integer read FReceiveTimeout write FReceiveTimeout default 0;
{:Determines whether the AdvHttp object raises an exception on http errors.}
    property  RaiseExceptions: boolean read FRaiseExceptions write SetRaiseExceptions
               default true;
{:HTTP user agent.}
    property  UserAgent:string read FUserAgent write SetUserAgent;
{:Full URL for the Proxy server.}
    property  Proxy : string read FProxy write SetProxy;
{:Turn On/Off Secure Socket Layers.
@seeAlso URL
@seeAlso HostName
@seeAlso URLPath
@seeAlso ExtraInfo
@seeAlso Port}
    property  SSL : boolean read FSSL write SetSSL default false;
{:Uniform Resourse Locator for the http request.
@seeAlso HostName
@seeAlso URLPath
@seeAlso ExtraInfo
@seeAlso Port
@seeAlso SSL}
    property  URL : string read FURL write SetURL;
{:URL path for the specified request.
@seeAlso URL
@seeAlso HostName
@seeAlso ExtraInfo
@seeAlso Port
@seeAlso SSL}
    property  URLPath : string read FURLPath write SetURLPath;
{:Additional URL information (for example, ?foo or #foo).
@seeAlso URL
@seeAlso HostName
@seeAlso URLPath
@seeAlso Port
@seeAlso SSL}
    property  ExtraInfo : string read FExtraInfo write SetExtraInfo;
{:Connection port. Default 80.
@seeAlso URL
@seeAlso HostName
@seeAlso URLPath
@seeAlso ExtraInfo
@seeAlso SSL}
    property  Port : Integer read FPort write SetPort default 80;
{:User name for server authentication.}
    property  UserName : string read FUserName write SetUserName;
{:Password for server authentication.}
    property  Password : string read FPassword write SetPassword;
{:User name for proxy authentication.
@seeAlso Proxy}
    property  ProxyUserName : string read FProxyUserName write SetProxyUserName;
{:Password for proxy authentication.
@seeAlso Proxy}
    property  ProxyPassword : string read FProxyPassword write SetProxyPassword;
{:Name of the web server.}
    property  HostName : string read FHostName write SetHostName;
{:If true, the Internet Explorer standard dialog will show up on server
or proxy authentication needs. Default true.}
    property  DefaultUi : Boolean read FDefaultUi write SetDefaultUi default True;
{:String list with the request headers will be sent to the web server. To add some header,
simply execute RequestHeaders.Add('<i>header</i>').
Example: RequestHeaders.Add('Accept-type: text/html');}
    property  RequestHeaders : TStringList read FRequestHeaders write SetRequestHeaders;
{:List of content types accepted by the client.
 If empty, no types are accepted by the client.
 Servers interpret a lack of accept types to indicate that the client accepts
 only documents of type "text/*" (that is, only text documents,
 and not pictures or other binary files).}
    property  AcceptTypes : TStringList read FAcceptTypes write SetAcceptTypes;
{:HTTP method. Valids methods are GET, POST, PUT, HEAD. Default 'GET'.}
    property  Method : string read FMethod write SetMethod;
{:Set the body of the request and returns the body of the response.
@seeAlso RequestStream
@seeAlso ResponseStream}
    property  Text : string read GetText write SetText;
{:Number of authorization retries.}
    property  AuthRetries:Integer read FAuthRetries write SetAuthRetries;
{:Several flags to be passed to wininet HttpOpenRequest method.
@SeeAlso TInternetFlags}
    property  InternetFlags : TInternetFlags read FInternetFlags write SetInternetFlags;
{:Occurs when closing the connection to the server.}
    property  OnClosingConnection : TNotifyEvent read FOnClosingConnection write SetOnClosingConnection;
{:Occurs if successfully connected to the server.}
    property  OnConnectedToServer : TNotifyEvent read FOnConnectedToServer write SetOnConnectedToServer;
{:Occurs while connecting to the server.}
    property  OnConnectingToServer : TNotifyEvent read FOnConnectingToServer write SetOnConnectingToServer;
{:Occurs if successfully closed the connection to the server.}
    property  OnConnectionClosed : TNotifyEvent read FOnConnectionClosed write SetOnConnectionClosed;
{:Occurs when received an intermediate (100 level) status code message from the server.}
    property  OnIntermediateResponse : TNotifyEvent read FOnIntermediateResponse write SetOnIntermediateResponse;
{:Occurs when successfully found the IP address of the name contained in HostName property.}
    property  OnNameResolved : TNotifyEvent read FOnNameResolved write SetOnNameResolved;
{:Occurs while waiting for the server to respond to a request.}
    property  OnReceivingResponse : TNotifyEvent read FOnReceivingResponse write SetOnReceivingResponse;
{:Occurs when successfully received a response from the server. The BytesReceived property contains
  the number of bytes received.}
    property  OnResponseReceived : TNotifyEvent read FOnResponseReceived write SetOnResponseReceived;
{:Occurs when a redirection is received from the server.}
    property  OnRedirect : TNotifyEvent read FOnRedirect write SetOnRedirect;
{:Occurs when the asynchronous operation has been completed.}
    property  OnRequestComplete : TNotifyEvent read FOnRequestComplete write SetOnRequestComplete;
{:Occurs when the request information is successfully sent to the server. The property
  BytesSent, containts the number of bytes sent.}
    property  OnRequestSent : TNotifyEvent read FOnRequestSent write SetOnRequestSent;
{:Occurs while looking up the IP address of the name contained in HostName property.}
    property  OnResolvingName : TNotifyEvent read FOnResolvingName write SetOnResolvingName;
{:Occurs while sending the information request to the server.}
    property  OnSendingRequest : TNotifyEvent read FOnSendingRequest write SetOnSendingRequest;
{:Occurs when moved between a secure (HTTPS) and a nonsecure (HTTP) site.}
    property  OnStateChange : TNotifyEvent read FOnStateChange write SetOnStateChange;
{:Occurs when new data has been received an is available in ResponseStream property.}
    property  OnDataAvailable : TNotifyEvent read FOnDataAvailable write SetOnDataAvailable;
{:Occurs when authentication information is needed. You can provide your own authentication
  dialog by setting to false the DefaultUI property, and filling the UserName and Password
  properties.
@seeAlso DefaultUi
@seeAlso UserName
@seeAlso Password}
    property  OnNeedAuth : TNotifyEvent read FOnNeedAuth write SetOnNeedAuth;
{:Occurs when proxy authentication information is needed. You can provide your own authentication
  dialog by setting to false the DefaultUI property, and filling the ProxyUserName and ProxyPassword
  properties.
@seeAlso DefaultUi
@seeAlso Proxy
@seeAlso ProxyUserName
@seeAlso ProxyPassword}
    property  OnNeedProxyAuth : TNotifyEvent read FOnNeedProxyAuth write SetOnNeedProxyAuth;
{:Occurs on an error in the request. The LastError property contains the Wininet or Win32 error code.]
  You can get an error description by calling the FormatMessage method.
@seeAlso LastError
@seeAlso FormatMessage}
    property  OnError : TNotifyEvent read FOnError write SetOnError;
  end;

procedure Register;

implementation
{$R *.DCr}

const
  CM_EXECPROC = $8FFF;
  CM_DESTROYWINDOW = $8FFE;
  CM_FREEPROC = $8FFD;

type
  PRaiseFrame = ^TRaiseFrame;
  TRaiseFrame = record
    NextRaise: PRaiseFrame;
    ExceptAddr: Pointer;
    ExceptObject: TObject;
    ExceptionRecord: PExceptionRecord;
  end;

var
  InetWindow: HWND;
  InetCount: Integer;


function fnRequestComplete(Context : Pointer):DWORD;cdecl;
begin
  TAdvHttp(Context).RequestComplete;
  SetEvent(TAdvHttp(Context).hFinish);
  result:=0;
end;

procedure FreeInetWindow;
begin
  if InetWindow <> 0 then
  begin
    DestroyWindow(InetWindow);
    InetWindow := 0;
  end;
end;

function InetWndProc(Window: HWND; Message, wParam, lParam: Longint): Longint; stdcall;
begin
  case Message of
    CM_EXECPROC:
      with TAdvHttp(lParam) do
      begin
        Result := 0;
        try
          FSynchronizeException := nil;
          FFunc(TObject(lParam));
        except
          if RaiseList <> nil then
          begin
            FSynchronizeException := PRaiseFrame(RaiseList)^.ExceptObject;
            PRaiseFrame(RaiseList)^.ExceptObject := nil;
          end;
        end;
      end;
    CM_FREEPROC: begin
        TAdvHttp(lParam).Free;
        result:=0;
      end;
    CM_DESTROYWINDOW:
      begin
        FreeInetWindow;
        Result := 0;
      end;
  else
    Result := DefWindowProc(Window, Message, wParam, lParam);
  end;
end;

var
  InetWindowClass: TWndClass = (
    style: 0;
    lpfnWndProc: @InetWndProc;
    cbClsExtra: 0;
    cbWndExtra: 0;
    hInstance: 0;
    hIcon: 0;
    hCursor: 0;
    hbrBackground: 0;
    lpszMenuName: nil;
    lpszClassName: 'TAdvHttpWindow');


function AllocateWindow: HWND;
var
  TempClass: TWndClass;
  ClassRegistered: Boolean;
begin
  InetWindowClass.hInstance := HInstance;
  ClassRegistered := GetClassInfo(HInstance, InetWindowClass.lpszClassName,
    TempClass);
  if not ClassRegistered or (TempClass.lpfnWndProc <> @InetWndProc) then
  begin
    if ClassRegistered then
      Windows.UnregisterClass(InetWindowClass.lpszClassName, HInstance);
    Windows.RegisterClass(InetWindowClass);
  end;
  Result := CreateWindow(InetWindowClass.lpszClassName, '', 0,
    0, 0, 0, 0, 0, 0, HInstance, nil);
end;

procedure glbCallback(hInt:HINTERNET; dwContext,dwInternetStatus:DWORD;
                      lpvStatusInformation:Pointer;dwStatusInformationLength:DWORD);stdcall;
begin
  TAdvHttp(dwContext).IstCallback(hInt,dwContext,dwInternetStatus,
                    lpvStatusInformation,dwStatusInformationLength);
end;

procedure Register;
begin
  RegisterComponents('Samples', [TAdvHttp]);
end;

{ TAdvHttp }

constructor TAdvHttp.create(aowner: TComponent);
begin
  inherited Create(Aowner);
  FUserAgent := 'TeamSoft WinInet Component';
  FRaiseExceptions := True;
  FSSL := False;
  FResponseHeaders:=TStringList.Create;
  FRequestHeaders:=TStringList.Create;
  FAcceptTypes:=TStringList.Create;
  FResponseStream := TMemoryStream.Create;
  FRequestStream := TMemoryStream.Create;
  FMethod:='GET';
  FHostName:='localhost';
  FPort:=80;
  FDefaultUi:=True;
  FAuthRetries:=3;
  if Assigned(Owner) and (Owner is TWinControl) then
     FSynchronizeEvents:=True
  else
     FSynchronizeEvents:=False;

  InternetFlags := [ifIgnoreCertCnInvalid,ifIgnoreCertDateInvalid,
                    ifIgnoreRedirectToHttp,ifIgnoreRedirectToHttps,
                    ifKeepConnection,ifNoUi,ifReload];
  if not (csDesigning in ComponentState) then begin
    if FSynchronizeEvents then begin
      if InetCount=0 then
        InetWindow:=AllocateWindow;
      Inc(InetCount);
    end;
  end;
  hFinish := CreateEvent(nil,true,false,nil);
end;

destructor TAdvHttp.destroy;
begin
  FCanceled:=True;
  if (hThread<>0) then
     WaitForSingleObject(hThread,INFINITE);
  FAcceptTypes.Free;
  FRequestStream.Free;
  FResponseStream.Free;
  UnInitialize;
  CloseHandle(hFinish);
  if not (csDesigning in ComponentState) then begin
    if FSynchronizeEvents then begin
      Dec(InetCount);
      if InetCount=0 then
        FreeInetWindow;//      PostMessage(InetWindow, CM_DESTROYWINDOW, 0, 0);
    end;
  end;
  FRequestHeaders.Free;
  FResponseHeaders.Free;
  inherited Destroy;
end;

procedure TAdvHttp.PostFree;
begin
  if (csDestroying in ComponentState) then exit;
  PostMessage(InetWindow, CM_FREEPROC, 0, Longint(Self));
end;

procedure TAdvHttp.Synchronize(Method: TNotifyEvent);
begin
  if (csDestroying in ComponentState) then exit;
  if FSynchronizeEvents then begin
    FSynchronizeException := nil;
    FFunc := Method;
    SendMessage(InetWindow, CM_EXECPROC, 0, Longint(Self));
    if Assigned(FSynchronizeException) then raise FSynchronizeException;
  end else Method(Self);
end;

function TAdvHttp.FormatMessage(error:DWORD):string;
var
   TmpErr   : DWORD;
   Len      : DWORD;
   Buf      : array[0..4096] of char;
   s        : string;
   lpszStrName : PChar;
begin
  lpszStrName := PChar(AllocMem(257*2));
  try
    windows.FormatMessage(FORMAT_MESSAGE_FROM_HMODULE,
         Pointer(GetModuleHandle('wininet.dll')),Error,0,
         lpszStrName,256,nil);

    s:=lpszStrName;
    if error = ERROR_INTERNET_EXTENDED_ERROR then begin
      Len := sizeof(Buf);
      InternetGetLastResponseInfo(TmpErr, Buf, Len);
      Buf[Len] := #0;
      s := Format('%s'#13#10'Extended error:%d - %s'#13#10, [s,TmpErr,Buf]);
    end;

    result:=s;

  finally
    FreeMem(lpszStrName);
  end;
end;

procedure TAdvHttp.INetCheck(b: LongBool);
begin
   SetLastError(0);
   if not b then begin
      SetLastError(GetLastError);
      if (FLastError = ERROR_IO_PENDING) or (FLastError=0)then exit;
      raise EInetError.Create(FormatMessage(FLastError));
   end;
end;

function TAdvHttp.Initialize: Boolean;
begin
  //default false
  result:=false;

  try
    if FProxy='' then begin
      //create the root HINTERNET handle using the systems default
      //settings.
      hOpen := InternetOpen(PChar(FUserAgent), INTERNET_OPEN_TYPE_PRECONFIG,
                            nil, nil, FSync);
    end else begin
      //create the root HINTERNET handle using the proxy config
      hOpen := InternetOpen(PChar(FUserAgent), INTERNET_OPEN_TYPE_PROXY,
                            PChar(FProxy), nil, FSync);
    end;

    //check if the root HINTERNET handle has been created
    InetCheck(hOpen<>nil);

    //sets the callback function
    FIscCallback := InternetSetStatusCallback(hOpen, @glbCallBack);

  except
    if FRaiseExceptions then raise;
  end;
end;

function TAdvHttp.UnInitialize: Boolean;
begin
  result:=true;
  Cleanup;
  //closes the root HINTERNET handle
  if Assigned(hOpen) then
    result := InternetCloseHandle(hOpen);
  hOpen     := nil;
end;

procedure TAdvHttp.SetUserAgent(const Value: string);
begin
  FUserAgent := Value;

  if Assigned(hOpen) then begin
    UnInitialize;
    Initialize;
  end;
end;

procedure TAdvHttp.SetProxy(const Value: string);
begin
  FProxy := Value;

  if Assigned(hOpen) then begin
    UnInitialize;
    Initialize;
  end;
end;

procedure TAdvHttp.SetSSL(const Value: boolean);
begin
  FSSL := Value;
  CombineUrl;
end;

procedure TAdvHttp.SetRaiseExceptions(const Value: boolean);
begin
  FRaiseExceptions := Value;
end;


function TAdvHttp.NeedAuth:boolean;
var
  dwStatus,cbStatus,
  dwReserved:DWORD;
  Log : ILogger;
begin
  Log := TLogger.Create(Self,'NeedAuth');
  result:=False;
  // Get status code.
  dwReserved:=0;
  cbStatus := sizeof(dwStatus);
  try
    InetCheck(HttpQueryInfo(hResource,
        HTTP_QUERY_FLAG_NUMBER or HTTP_QUERY_STATUS_CODE,
        @dwStatus,
        cbStatus,
        dwReserved));
  except
    exit;
  end;

  // Look for 401 or 407.
  FHTTPStatus:=dwStatus;
  case dwStatus of
    HTTP_STATUS_DENIED: begin
        If Assigned(FOnNeedAuth) then
          Synchronize(FOnNeedAuth);
        result:=True;
      end;
    HTTP_STATUS_PROXY_AUTH_REQ: begin
        If Assigned(FOnNeedProxyAuth) then
           Synchronize(FOnNeedProxyAuth);
        result:=True;
      end;
  end;
end;

function TAdvHttp.CombineUrl:boolean;
var BaseUrl:string;
    lpszBuffer : PChar;
    dwBufferLength: DWORD;
    RelativeUrl:string;
begin
  if FSSL then BaseUrl:='https://'+FHostName
  else BaseUrl:='http://'+FHostName;

  if FPort<>80 then BaseUrl:=BaseUrl+':'+IntToStr(FPort);

  if FExtraInfo<>'' then RelativeUrl:=FURLPath+FExtraInfo
  else                   RelativeUrl:=FURLPath;

  lpszBuffer:=AllocMem(INTERNET_MAX_URL_LENGTH*2);
  dwBufferLength:=INTERNET_MAX_URL_LENGTH;
  try
    result:=InternetCombineUrl(PChar(BaseUrl), PChar(RelativeUrl),
              lpszBuffer,dwBufferLength,ICU_ENCODE_SPACES_ONLY);
    InetCheck(result);

    FUrl:=lpszBuffer;
  finally
    FreeMem(lpszBuffer);
  end;
end;

function TAdvHttp.CrackUrl:boolean;
var UrlComponents:TURLComponents;
begin
  FillChar(URLComponents,sizeof(URLComponents),0);
  with URLComponents do begin
    dwStructSize:= sizeof(URLComponents);
    lpszHostName:=allocmem(INTERNET_MAX_HOST_NAME_LENGTH*2);
    dwHostNameLength:=INTERNET_MAX_HOST_NAME_LENGTH;
    lpszUserName:=allocmem(INTERNET_MAX_USER_NAME_LENGTH*2);
    dwUserNameLength:=INTERNET_MAX_USER_NAME_LENGTH;
    lpszPassword:=allocmem(INTERNET_MAX_USER_NAME_LENGTH*2);
    dwPasswordLength:=INTERNET_MAX_USER_NAME_LENGTH;
    lpszUrlPath:=allocmem(INTERNET_MAX_PATH_LENGTH*2);
    dwUrlPathLength:=INTERNET_MAX_PATH_LENGTH;
    lpszExtraInfo:=allocmem(INTERNET_MAX_PATH_LENGTH*2);
    dwExtraInfoLength:=INTERNET_MAX_PATH_LENGTH;
  end;

  try
    result:=InternetCrackUrl(PChar(FUrl),Length(FUrl),ICU_ESCAPE,URLComponents);
    InetCheck(Result);

    with URLComponents do begin
      FHostName:=lpszHostName;
      FURLPath:=lpszURLPath;
      FExtraInfo:=lpszExtraInfo;
      if nScheme = INTERNET_SCHEME_HTTPS then
         SSL:=TRUE
      else if nScheme <> INTERNET_SCHEME_HTTP then
         raise Exception.Create('Invalid protocol');

      FPort:=nPort;
      if dwUserNameLength>0 then
        FUserName:=lpszUserName;
      if dwPasswordLength>0 then
        FPassword:=lpszPassword;
    end;
  finally
    FreeMem(URLComponents.lpszHostName);
    FreeMem(URLComponents.lpszUserName);
    FreeMem(URLComponents.lpszPassword);
    FreeMem(URLComponents.lpszURLPath);
    FreeMem(URLComponents.lpszExtraInfo);
  end;
end;

procedure TAdvHttp.Stop;
begin
  FCanceled:=True;
  if (hThread<>0) then
    ResumeThread(hThread)
  else CleanUp;
end;

procedure TAdvHttp.Cleanup;
begin
  if Assigned(hResource) then
     InternetCloseHandle(hResource);
  if Assigned(hConnect) then
     InternetCloseHandle(hConnect);
  if (hThread<>0) then
     CloseHandle(hThread);
  hResource:=nil;
  hConnect:=nil;
  hThread:=0;
  FCanceled:=False;
  Busy:=False;
end;

function TAdvHttp.StartSync(Timeout:Integer=0;RcvTimeOut:Integer=0): boolean;
var
  Log : ILogger;
begin
  Log := TLogger.Create(Self,'StartSync');
  result:=Start(True,TimeOut,RcvTimeOut) and (FLastError = 0);
end;

function TAdvHttp.Start(Sync:Boolean=False;TimeOut:Integer=0;RcvTimeOut:Integer=0): boolean;
var SecureFlag : DWORD;
    aAcceptTypes : array[0..100] of string;
    n : Integer;
    FullUrl : string;
    BeginTime: TDateTime;
    TotalTimeout: Integer;
  Log : ILogger;
begin
  Log := TLogger.Create(Self,'Start');
  ResetEvent(hFinish);
  BeginTime := Now;
  FSync:=0;
  if not Sync or (FSynchronizeMode = smWaitFor) then
    FSync:=INTERNET_FLAG_ASYNC;
  Busy:=True;
  result:=True;
  FCanceled:=False;
  if FSSL then SecureFlag:=INTERNET_FLAG_SECURE
  else SecureFlag:=0;

  FTotalBytesReceived:=0;
  FTotalBytesAvailable:=0;
  FBytesReceived:=0;
  FBytesAvailable:=0;
  FResponseStream.Clear;

  If TimeOut=0 then TimeOut := FConnectTimeOut;
  If RcvTimeOut=0 then RcvTimeOut := FReceiveTimeOut;

  if not Assigned(hOpen) then Initialize;
  try
    // Connect to host.
    hConnect := InternetConnect( hOpen,PChar(FHostName),FPort,nil,nil,
                                 INTERNET_SERVICE_HTTP,0,0);
    InetCheck(LongBool(hConnect));

    if TimeOut>0 then
      InetCheck(InternetSetOption(hConnect,INTERNET_OPTION_CONNECT_TIMEOUT,
                      @TimeOut,sizeof(DWORD)));
    if RcvTimeOut>0 then
      InetCheck(InternetSetOption(hConnect,INTERNET_OPTION_RECEIVE_TIMEOUT,
                      @RcvTimeOut,sizeof(DWORD)));

    FullUrl:=FUrlPath+FExtraInfo;  //GMR 30/06/99
    if AcceptTypes.Text='' then
      hResource := HttpOpenRequest(hConnect,PChar(FMethod),PChar(FullUrl),nil,
                      nil,nil,SecureFlag or FIFlags, DWORD(self))
    else begin
      for n:=0 to AcceptTypes.Count-1 do
        if (Length(AcceptTypes[n])>0) and (n<100) then
           aAcceptTypes[n]:=AcceptTypes[n];
      hResource := HttpOpenRequest(hConnect,PChar(FMethod),PChar(FullUrl),nil,
                    nil,@aAcceptTypes,SecureFlag or FIFlags, DWORD(self));
    end;

    InetCheck(LongBool(hResource));

    FAuthCount:=0;

// Added 3-2-99 GMR: Uses applies then username and password with
// SetInternetOption
    UserName:=FUserName;
    Password:=FPassword;
//

    SendRequest;
    
    if Sync and (FSynchronizeMode = smWaitFor) then
    begin
      TotalTimeout := Timeout + RcvTimeout;
      while (TotalTimeout <= 0) or (DateTimeToTimeStamp(Now - BeginTime).Time <= TotalTimeout) do
      begin
        if WaitForSingleObject(hFinish, 10) = WAIT_OBJECT_0 then Break;
        Application.ProcessMessages;
      end;
    end;
  except
    result:=False;
    if FRaiseExceptions then raise;
  end;
end;

procedure TAdvHttp.SendRequest;
var Headers : string;
  Log : ILogger;
begin
  Log := TLogger.Create(Self,'SendRequest');
  Headers:='';
  if FRequestHeaders.Count>0 then
    Headers:=FRequestHeaders.Text+#13#10;

  if Length(Headers)>0 then
    InetCheck(HttpSendRequest(hResource,PChar(Headers),Length(Headers),
              FRequestStream.Memory,FRequestStream.Size))
  else
    InetCheck(HttpSendRequest(hResource,nil,0,
              FRequestStream.Memory,FRequestStream.Size));
  if FSync=0 then
    RequestComplete;
end;

procedure TAdvHttp.SetURL(const Value: string);
var OldUrl : string;
begin
  try
    OldUrl:=FUrl;
    FURL := Value;
    CrackUrl;
  except
    FURL:=OldUrl;
    if FRaiseExceptions then raise;
  end
end;

procedure TAdvHttp.RequestComplete;
var dwErr : DWORD;
   p : Pointer;
  Log : ILogger;
begin
  Log := TLogger.Create(Self,'RequestComplete');
  try
//    if (PInternetAsyncResult(FStatusInformation)^.dwError = 0) then begin
      if NeedAuth then begin
        if (FAuthCount<FAuthRetries) then begin
          if FDefaultUi then begin
            dwErr := InternetErrorDlg(GetDesktopWindow,hResource,
                    FLastError,FLAGS_ERROR_UI_FILTER_FOR_ERRORS  or
                    FLAGS_ERROR_UI_FLAGS_CHANGE_OPTIONS or
                    FLAGS_ERROR_UI_FLAGS_GENERATE_DATA,p);

            if (dwErr = ERROR_SUCCESS) or
               (dwErr = ERROR_CANCELLED) then begin
              FLastError:=ERROR_INTERNET_OPERATION_CANCELLED;
              if Assigned(FOnError) then
                 Synchronize(FOnError);
              Exit;
            end;
          end;
          Inc(FAuthCount);

          if FCanceled then begin
            Cleanup;
            exit;
          end;

//          SendRequest;
//          Exit;

        end else begin
          FLastError:=ERROR_INTERNET_OPERATION_CANCELLED;
          if Assigned(FOnError) then
             Synchronize(FOnError);
          Exit;
        end;

      end;

      try
        GetHeaders;
        GetBody;
      finally
        if FLastError<>ERROR_INTERNET_OPERATION_CANCELLED then begin
          if Assigned(FOnRequestComplete) then
             Synchronize(FOnRequestComplete);
        end else begin
          Log.LogError('ERROR_INTERNET_OPERATION_CANCELLED');
        end;
      end;

//    end else SetLastError(GetLastError);
  finally
    hThread:=0;
  end;
end;

procedure TAdvHttp.SetProxyPassword(const Value: string);
begin
  FProxyPassword := Value;
  if Assigned(hConnect) then begin
    InternetSetOption(hConnect, INTERNET_OPTION_PROXY_PASSWORD,
        PChar(FProxyPassword), Length(FProxyPassword));
  end;
end;

procedure TAdvHttp.SetProxyUserName(const Value: string);
begin
  FProxyUserName := Value;
  if Assigned(hConnect) then begin
    InternetSetOption(hConnect, INTERNET_OPTION_PROXY_USERNAME,
        PChar(FProxyUserName), Length(FProxyUserName));
  end;
end;

procedure TAdvHttp.SetOnNeedAuth(const Value: TNotifyEvent);
begin
  FOnNeedAuth := Value;
end;

procedure TAdvHttp.SetOnNeedProxyAuth(const Value: TNotifyEvent);
begin
  FOnNeedProxyAuth := Value;
end;

procedure TAdvHttp.SetDefaultUi(const Value: Boolean);
begin
  FDefaultUi := Value;
end;


procedure TAdvHttp.IstCallback(hInt: HINTERNET; dwContext,
  dwInternetStatus: DWORD; lpvStatusInformation: Pointer;
  dwStatusInformationLength: DWORD);
var
  Log : ILogger;
begin
  Log := TLogger.Create(Self,'IstCallback dwInternetStatus:'+IntToStr(dwInternetStatus));
  FStatusInformation := lpvStatusInformation;
  FStatusInformationLength := dwStatusInformationLength;

  case dwInternetStatus of
    INTERNET_STATUS_CLOSING_CONNECTION:
      if Assigned(FOnClosingConnection) then
         Synchronize(FOnClosingConnection);
    INTERNET_STATUS_CONNECTED_TO_SERVER: begin
        FHostAddress:=PAnsiChar(FStatusInformation);
        if Assigned(FOnConnectedToServer) then
           Synchronize(FOnConnectedToServer);
      end;
    INTERNET_STATUS_CONNECTING_TO_SERVER: begin
        FHostAddress:=PAnsiChar(FStatusInformation);
        if Assigned(FOnConnectingToServer) then
           Synchronize(FOnConnectingToServer);
      end;
    INTERNET_STATUS_CONNECTION_CLOSED:
      if Assigned(FOnConnectionClosed) then
         Synchronize(FOnConnectionClosed);
    INTERNET_STATUS_HANDLE_CLOSING: begin
        if FFreeOnTerminate then
           PostFree;
        SetEvent(hFinish);
      end;
    INTERNET_STATUS_HANDLE_CREATED:;
    INTERNET_STATUS_INTERMEDIATE_RESPONSE:
      if Assigned(FOnIntermediateResponse) then
         Synchronize(FOnIntermediateResponse);
    INTERNET_STATUS_NAME_RESOLVED: begin
      FHostAddress:=PAnsiChar(FStatusInformation);
        if Assigned(FOnNameResolved) then
           Synchronize(FOnNameResolved);
      end;
    INTERNET_STATUS_RECEIVING_RESPONSE:
      if Assigned(FOnReceivingResponse) then
         Synchronize(FOnReceivingResponse);
    INTERNET_STATUS_RESPONSE_RECEIVED: begin
        FBytesReceived:=PDWORD(FStatusInformation)^;
        FTotalBytesReceived:=FTotalBytesReceived+FBytesReceived;
        if Assigned(FOnResponseReceived) then
          Synchronize(FOnResponseReceived);
      end;
    INTERNET_STATUS_REDIRECT: begin
        FRedirectedUrl:=PAnsiChar(FStatusInformation);
        if Assigned(FOnRedirect) then
           Synchronize(FOnRedirect);
      end;
    INTERNET_STATUS_REQUEST_COMPLETE: begin
        if PInternetAsyncResult(FStatusInformation)^.dwResult=0 then begin
          SetLastError(PInternetAsyncResult(FStatusInformation)^.dwError);
          SetEvent(hFinish);
        end else begin
          if (hThread<>0) then ResumeThread(hThread)
          else
          hThread := CreateThread(nil, 8192,TFNThreadStartRoutine(@fnRequestComplete),
                    Pointer(Self),0,FThreadID);
        end;
      end;
    INTERNET_STATUS_REQUEST_SENT: begin
        FBytesSent:=PDWORD(FStatusInformation)^;
        if Assigned(FOnRequestSent) then
           Synchronize(FOnRequestSent);
      end;
    INTERNET_STATUS_RESOLVING_NAME:
      if Assigned(FOnResolvingName) then
         Synchronize(FOnResolvingName);
    INTERNET_STATUS_SENDING_REQUEST:
      if Assigned(FOnSendingRequest) then
         Synchronize(FOnSendingRequest);
    INTERNET_STATUS_STATE_CHANGE:
      if Assigned(FOnStateChange) then
         Synchronize(FOnStateChange);
  end;
end;

procedure TAdvHttp.SetOnClosingConnection(const Value: TNotifyEvent);
begin
  FOnClosingConnection := Value;
end;

procedure TAdvHttp.SetOnConnectedToServer(const Value: TNotifyEvent);
begin
  FOnConnectedToServer := Value;
end;

procedure TAdvHttp.SetOnConnectingToServer(const Value: TNotifyEvent);
begin
  FOnConnectingToServer := Value;
end;

procedure TAdvHttp.SetOnConnectionClosed(const Value: TNotifyEvent);
begin
  FOnConnectionClosed := Value;
end;

procedure TAdvHttp.SetOnIntermediateResponse(const Value: TNotifyEvent);
begin
  FOnIntermediateResponse := Value;
end;

procedure TAdvHttp.SetOnNameResolved(const Value: TNotifyEvent);
begin
  FOnNameResolved := Value;
end;

procedure TAdvHttp.SetOnReceivingResponse(const Value: TNotifyEvent);
begin
  FOnReceivingResponse := Value;
end;

procedure TAdvHttp.SetOnRedirect(const Value: TNotifyEvent);
begin
  FOnRedirect := Value;
end;

procedure TAdvHttp.SetOnRequestComplete(const Value: TNotifyEvent);
begin
  FOnRequestComplete := Value;
end;

procedure TAdvHttp.SetOnRequestSent(const Value: TNotifyEvent);
begin
  FOnRequestSent := Value;
end;

procedure TAdvHttp.SetOnResolvingName(const Value: TNotifyEvent);
begin
  FOnResolvingName := Value;
end;

procedure TAdvHttp.SetOnResponseReceived(const Value: TNotifyEvent);
begin
  FOnResponseReceived := Value;
end;

procedure TAdvHttp.SetOnSendingRequest(const Value: TNotifyEvent);
begin
  FOnSendingRequest := Value;
end;

procedure TAdvHttp.SetOnStateChange(const Value: TNotifyEvent);
begin
  FOnStateChange := Value;
end;

procedure TAdvHttp.SetOnDataAvailable(const Value: TNotifyEvent);
begin
  FOnDataAvailable := Value;
end;

procedure TAdvHttp.GetHeaders;
var
  dwSize,dwReserved : DWORD;
  szHeaders : PChar;
  Log : ILogger;
begin
  Log := TLogger.Create(Self,'GetHeaders');
  try
    if hResource=nil then exit;
    szHeaders:=AllocMem(8192*2);
    try
      dwSize:=8192;
      dwReserved:=0;
      InetCheck(HttpQueryInfo(hResource,HTTP_QUERY_RAW_HEADERS_CRLF,
                PChar(szHeaders),dwSize,dwReserved));
      FResponseHeaders.Clear;
      if FLastError<>0 then exit;
      FResponseHeaders.Text:=szHeaders;
    finally
      FreeMem(szHeaders);
    end;
  except
    if FRaiseExceptions then raise;
  end;
end;

procedure TAdvHttp.GetBody;
var
  dwSize,dwDownloaded : DWORD;
  szData : PAnsiChar;
  Log : ILogger;
begin
  Log := TLogger.Create(Self,'GetBody');
  dwSize := 16384;
  szData := AllocMem(dwSize+1);
  try
    try
      FResponseStream.Clear;
      // The call to InternetQueryDataAvailable determines the amount of
      // data available to download.
      while (true) do begin
        if hResource=nil then exit;
        if FCanceled then exit;

        InetCheck(InternetReadFile(hResource,szData,dwSize,dwDownloaded));

        if (FLastError=ERROR_IO_PENDING) then
          SuspendTHread(hThread);

        if dwDownloaded=0 then exit;

        FLastStreamPointer:=FResponseStream.Position;
        FResponseStream.Write(szData^,dwDownloaded);
        FBytesAvailable := dwDownloaded;
        FTotalBytesAvailable:=FTotalBytesAvailable+FBytesAvailable;
        If Assigned (FOnDataAvailable) then
           Synchronize(FOnDataAvailable);
      end;
    except
      if FRaiseExceptions then raise;
    end;
  finally
    FreeMem(szData);
    Cleanup;
  end;
end;

function TAdvHttp.GetText: AnsiString;
begin
  SetLength(result,FResponseStream.Size);
  move(FResponseStream.Memory^,result[1], ResponseStream.Size);
end;

procedure TAdvHttp.SetText(text: String);
var
  aux : AnsiString;
begin
  FRequestStream.Clear;
  aux:=Text;
  FRequestStream.Write(aux[1],Length(aux));
end;

procedure TAdvHttp.SetMethod(const Value: string);
begin
  FMethod := Value
end;

procedure TAdvHttp.SetPassword(const Value: string);
begin
  FPassword := Value;
  if Assigned(hConnect) then begin
    InternetSetOption(hConnect, INTERNET_OPTION_PASSWORD,
        PChar(FPassword), Length(FPassword));
  end;
end;

procedure TAdvHttp.SetUserName(const Value: string);
begin
  FUserName := Value;
  if Assigned(hConnect) then begin
    InternetSetOption(hConnect, INTERNET_OPTION_USERNAME,
        PChar(FUserName), Length(FUserName));
  end;
end;

procedure TAdvHttp.SetHostName(const Value: string);
var OldHostname:string;
begin
  try
    OldHostName:=FHostname;
    FHostName := Value;
    CombineUrl;
  except
    FHostName:=OldHostName;
    if FRaiseExceptions then raise;
  end;
end;

procedure TAdvHttp.SetPort(const Value: Integer);
begin
  FPort := Value;
  CombineUrl;
end;

procedure TAdvHttp.SetLastError(const Value: DWORD);
begin
  FLastError := Value;
  if (FLastError<>0) and (FLastError<>ERROR_IO_PENDING) and
     Assigned(FOnError) then
        Synchronize(FOnError);
end;

procedure TAdvHttp.SetOnError(const Value: TNotifyEvent);
begin
  FOnError := Value;
end;

procedure TAdvHttp.SetURLPath(const Value: string);
var OldUrlPath:string;
begin
  try
    OldURLPath:=FURLPath;
    FURLPath := Value;
    CombineUrl;
  except
    FURLPath:=OldURLPath;
    if FRaiseExceptions then raise;
  end;
end;

procedure TAdvHttp.SetExtraInfo(const Value: string);
var OldExtraInfo:string;
begin
  try
    OldExtraInfo:=FExtraInfo;

    if (Length(Value)>0) and (Value[1]<>'#') and
       (Value[1]<>'?') then FExtraInfo:='?'+Value
    else  FExtraInfo := Value;
    CombineUrl;
  except
    FExtraInfo:=OldExtraInfo;
    if FRaiseExceptions then raise;
  end;
end;

function  TAdvHttp.GetSuccess:Boolean;
begin
  result:=FHTTPStatus=200;
end;

procedure TAdvHttp.SetInternetFlags(const Value: TInternetFlags);
begin
  FInternetFlags := Value;
  FIFlags:=0;
  if ifCacheIfNetFail in FInternetFlags then
     FIFlags:=FIFlags or INTERNET_FLAG_CACHE_IF_NET_FAIL;

  if ifDontCache in FInternetFlags then
     FIFlags:=FIFlags or INTERNET_FLAG_DONT_CACHE;

  if ifHyperLink in FInternetFlags then
     FIFlags:=FIFlags or INTERNET_FLAG_HYPERLINK;

  if ifIgnoreCertCnInvalid in FInternetFlags then
     FIFlags:=FIFlags or INTERNET_FLAG_IGNORE_CERT_CN_INVALID;

  if ifIgnoreCertDateInvalid in FInternetFlags then
     FIFlags:=FIFlags or INTERNET_FLAG_IGNORE_CERT_DATE_INVALID;

  if ifIgnoreRedirectToHttp in FInternetFlags then
     FIFlags:=FIFlags or INTERNET_FLAG_IGNORE_REDIRECT_TO_HTTP;

  if ifIgnoreRedirectToHttps in FInternetFlags then
     FIFlags:=FIFlags or INTERNET_FLAG_IGNORE_REDIRECT_TO_HTTPS;

  if ifKeepConnection in FInternetFlags then
     FIFlags:=FIFlags or INTERNET_FLAG_KEEP_CONNECTION;

  if ifNeedFile in FInternetFlags then
     FIFlags:=FIFlags or INTERNET_FLAG_NEED_FILE;

  if ifNoAuth in FInternetFlags then
     FIFlags:=FIFlags or INTERNET_FLAG_NO_AUTH;

  if ifNoAutoRedirect in FInternetFlags then
     FIFlags:=FIFlags or INTERNET_FLAG_NO_AUTO_REDIRECT;

  if ifNoCacheWrite in FInternetFlags then
     FIFlags:=FIFlags or INTERNET_FLAG_NO_CACHE_WRITE;

  if ifNoCookies in FInternetFlags then
     FIFlags:=FIFlags or INTERNET_FLAG_NO_COOKIES;

  if ifNoUi in FInternetFlags then
     FIFlags:=FIFlags or INTERNET_FLAG_NO_UI;

  if ifPragmaNoCache in FInternetFlags then
     FIFlags:=FIFlags or INTERNET_FLAG_PRAGMA_NOCACHE;

  if ifReload in FInternetFlags then
     FIFlags:=FIFlags or INTERNET_FLAG_RELOAD;

  if ifResynchronize in FInternetFlags then
     FIFlags:=FIFlags or INTERNET_FLAG_RESYNCHRONIZE;
end;

procedure TAdvHttp.SetAuthRetries(const Value: Integer);
begin
  FAuthRetries := Value;
end;

procedure TAdvHttp.SetAcceptTypes(const Value: TStringList);
begin
  FAcceptTypes.Assign(Value);
end;

procedure TAdvHttp.SetRequestHeaders(const Value: TStringList);
begin
  FRequestHeaders.Assign(value);
end;

procedure TAdvHttp.SetFreeOnTerminate(const Value: Boolean);
begin
  FFreeOnTerminate := Value;
end;

procedure TAdvHttp.SetConnectTimeout(const Value: integer);
begin
  FConnectTimeout := Value;
end;

procedure TAdvHttp.SetSynchronizeEvents(const Value: Boolean);
begin
  FSynchronizeEvents := Value;
end;

procedure TAdvHttp.SetSynchronizeMode(const Value: TSynchronizeMode);
begin
  FSynchronizeMode := Value;
end;

procedure TAdvHttp.SetBusy(const Value: Boolean);
begin
  FBusy := Value;
end;

end.






