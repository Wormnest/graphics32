{$IFDEF FPC}
  // Copied from newer GR32
  {$DEFINE COMPILERFPC}
  {$DEFINE PLATFORM_INDEPENDENT}
  {$DEFINE INLININGSUPPORTED}

  // Set up internal CPU target directives according to FPC directives
  {$IFDEF CPU386}
    {$IFDEF CPUI386}
      // target is an Intel 80386 or later.
      {$DEFINE TARGET_x86}
      {$ASMMODE INTEL}
    {$ENDIF}

    {$IFDEF CPUX86_64}
      // target is a 64-bit processor (AMD or INTEL).
      {$DEFINE TARGET_x64}
      {$ASMMODE INTEL}
    {$ENDIF}

    {$IFDEF CPUPOWERPC}
      // target is a 32-bit PowerPC or compatible.
      // currently only indirect support, added here as example on how to add
      // future specific targets
      {$DEFINE TARGET_POWERPC}
    {$ENDIF}
  {$ENDIF}

  {$IFDEF CPUX86_64}
    // target is a 64-bit processor (AMD or INTEL).
    {$DEFINE TARGET_x64}
    {$ASMMODE INTEL}
  {$ENDIF}

  {$IFDEF CPUARM}
    // target is an ARM processor.
    {$DEFINE TARGET_ARM}
  {$ENDIF}

  // Widgetsets other then Windows will want to implement BITS_GETTER
  {$IFNDEF LCLWin32}
    {$DEFINE BITS_GETTER}
  {$ENDIF}

  {$IFDEF PUREPASCAL}
    {$DEFINE NATIVE_SINCOS}
  {$ENDIF}
  {$IFDEF TARGET_x64}
    {$DEFINE NATIVE_SINCOS}
  {$ENDIF}

  {$IFDEF FPC}
    {$DEFINE HAS_NATIVEINT}
  {$ENDIF}

  {$UNDEF USETHREADRESUME}

  // end copied

  {$mode delphi}
//  {$DEFINE INLININGSUPPORTED}
  {$DEFINE COMPILER2005}
  {$DEFINE COMPILER7}
  {$DEFINE COMPILER6}
  {$DEFINE COMPILER5}
  {$DEFINE COMPILER4}
{$ENDIF}

{$IFDEF VER310} // Delphi 10.1 Berlin
  {$DEFINE TARGET_x86}
  {$DEFINE INLININGSUPPORTED}
  {$DEFINE COMPILER2005}
  {$DEFINE COMPILER7}
  {$DEFINE COMPILER6}
  {$DEFINE COMPILER5}
  {$DEFINE COMPILER4}
  {$IFNDEF BCB}
    {$DEFINE DELPHI2005}
  {$ELSE}
    {$DEFINE BCB7}
  {$ENDIF}
{$ENDIF}

{$IFDEF VER170}
  {$DEFINE TARGET_x86}
  {$DEFINE INLININGSUPPORTED}
  {$DEFINE COMPILER2005}
  {$DEFINE COMPILER7}
  {$DEFINE COMPILER6}
  {$DEFINE COMPILER5}
  {$DEFINE COMPILER4}
  {$IFNDEF BCB}
    {$DEFINE DELPHI2005}
  {$ELSE}
    {$DEFINE BCB7}
  {$ENDIF}
{$ENDIF}

{$IFDEF VER150}
  {$DEFINE TARGET_x86}
  {$DEFINE COMPILER7}
  {$DEFINE COMPILER6}
  {$DEFINE COMPILER5}
  {$DEFINE COMPILER4}
  {$IFNDEF BCB}
    {$DEFINE DELPHI7}
  {$ELSE}
    {$DEFINE BCB7}
  {$ENDIF}
{$ENDIF}

{$IFDEF VER140}
  {$DEFINE TARGET_x86}
  {$DEFINE COMPILER6}
  {$DEFINE COMPILER5}
  {$DEFINE COMPILER4}
  {$IFNDEF BCB}
    {$DEFINE DELPHI6}
  {$ELSE}
    {$DEFINE BCB6}
  {$ENDIF}
{$ENDIF}

{$IFDEF VER130}
  {$DEFINE TARGET_x86}
  {$DEFINE COMPILER5}
  {$DEFINE COMPILER4}
  {$IFNDEF BCB}
    {$DEFINE DELPHI5}
    {$DEFINE DELPHI4}
  {$ELSE}
    {$ObjExportAll On}
    {$DEFINE BCB5}
    {$DEFINE BCB4}
  {$ENDIF}
{$ENDIF}

{$IFDEF VER125}
  {$DEFINE TARGET_x86}
  {$DEFINE COMPILER4}
  {$DEFINE BCB4}
{$ENDIF}

{$IFDEF VER120}
  {$DEFINE TARGET_x86}
  {$DEFINE COMPILER4}
  {$DEFINE DELPHI4}
{$ENDIF}

{$IFNDEF COMPILER4}
//////////////////////////////////////////////////////////////////////
'  Graphics32 may only be installed with Delphi 4, Delphi 5,         '
'  Delphi6, Delphi7, C++ Builder 4, C++ Builder 5, or C++Builder 6 ! '
//////////////////////////////////////////////////////////////////////
{$ENDIF}

{$IFDEF DELPHI5}
  {$DEFINE EXT_PROP_EDIT}
{$ENDIF}

{$IFDEF LINUX}
  {$DEFINE CLX}
{$ENDIF}

{$IFDEF INLININGSUPPORTED}
  // comment the following define out if you want to avoid function inlining
  // which might increase executable size but will give better performance in
  // most cases.
  {$DEFINE USEINLINING}
{$ENDIF}

//{$DEFINE CLX}  // uncomment this if you want CLX for Windows

{$R-}{$Q-}  // switch off overflow and range checking

{$IFNDEF FPC}
{$DEFINE XPTHEMES} // enable support for windows xp themes
{$ELSE}
  {$IFDEF WINDOWS}
  {$DEFINE XPTHEMES} // enable support for windows xp themes
  {$ENDIF}
{$ENDIF}

