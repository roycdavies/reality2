%% This is an -*- erlang -*- file.
%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2010-2023. All Rights Reserved.
%%
%% Licensed under the Apache License, Version 2.0 (the "License");
%% you may not use this file except in compliance with the License.
%% You may obtain a copy of the License at
%%
%%     http://www.apache.org/licenses/LICENSE-2.0
%%
%% Unless required by applicable law or agreed to in writing, software
%% distributed under the License is distributed on an "AS IS" BASIS,
%% WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
%% See the License for the specific language governing permissions and
%% limitations under the License.
%%
%% %CopyrightEnd%

{application, wx,
 [{description, "Yet another graphics system"},
  {vsn, "2.3.1"},
  {modules,
   [
    %% Generated modules
  wxGraphicsContext, wxPreviewFrame, wxFontPickerCtrl, wxFileDialog, wxFlexGridSizer, wxSpinEvent, wxPrintDialogData, wxTopLevelWindow, wxFocusEvent, wxScrolledWindow, wxDisplay, wxDCOverlay, wxClipboardTextEvent, wxMoveEvent, wxChoicebook, wxSystemOptions, wxNotificationMessage, wxGridCellFloatRenderer, wxWindowDC, wxColourDialog, wxHtmlLinkEvent, wxStatusBar, wxInitDialogEvent, wxEvent, wxTaskBarIconEvent, wxGraphicsObject, wxPrintout, wxSysColourChangedEvent, wxGridCellRenderer, wxListCtrl, wxLocale, wxGraphicsMatrix, wxBitmap, wxQueryNewPaletteEvent, wxRegion, wxSizerItem, wxFrame, wxNavigationKeyEvent, wxGraphicsRenderer, wxGridCellBoolRenderer, wxMouseCaptureLostEvent, wxTextEntryDialog, wxIdleEvent, wxStyledTextCtrl, wxChoice, wxListItem, wxSpinCtrl, wxControlWithItems, wxMDIChildFrame, wxStdDialogButtonSizer, wxPrintData, wxDirPickerCtrl, wxKeyEvent, wxEraseEvent, wxFontDialog, wxRadioBox, wxCalendarDateAttr, wxPaintEvent, wxGridCellEditor, wxWebViewEvent, wxPalette, wxTreebook, wxWebView, wxLogNull, wxGraphicsBrush, wxWindowDestroyEvent, wxSetCursorEvent, wxMenuItem, wxMirrorDC, wxToggleButton, wxGraphicsFont, wxStaticText, wxBufferedDC, wxControl, wxCalendarCtrl, wxIconizeEvent, wxPostScriptDC, wxJoystickEvent, wxStaticBitmap, wxGridBagSizer, wxGridSizer, wxScrollEvent, wxGLContext, wxWindowCreateEvent, wxSashLayoutWindow, wxGridCellFloatEditor, wxStyledTextEvent, wxPrintDialog, wxStaticBox, wxDateEvent, wxTextCtrl, wxGridCellAttr, wxMDIClientWindow, wxGridCellTextEditor, wxDataObject, wxPrintPreview, wxFindReplaceDialog, wxCalendarEvent, wxCommandEvent, wxGraphicsPath, wxStaticLine, wxMiniFrame, wxDisplayChangedEvent, wxListEvent, wxDialog, wxBrush, wxCheckListBox, wxTreeCtrl, wxScreenDC, wxFileDataObject, wxPopupWindow, wxChildFocusEvent, wxColourPickerCtrl, wxFilePickerCtrl, wxGrid, wxAuiSimpleTabArt, wxSashEvent, wxSizerFlags, wxMask, wxFontData, wxScrollBar, wxBookCtrlEvent, wxCheckBox, wxHtmlWindow, wxPaletteChangedEvent, wxListItemAttr, wxGraphicsGradientStops, wxAuiManager, wxBoxSizer, wxClipboard, wxMouseEvent, wxMenu, wxAuiPaneInfo, wxPaintDC, wxSplitterWindow, wxProgressDialog, wxGridCellNumberEditor, wxListBox, wxActivateEvent, wxFileDirPickerEvent, wxCursor, wxMessageDialog, wxButton, wxMenuBar, wxMaximizeEvent, wxToolBar, wxGraphicsPen, wxGridCellNumberRenderer, wxNotifyEvent, wxArtProvider, wxHtmlEasyPrinting, wxNotebook, wxBufferedPaintDC, wxFindReplaceData, wxListView, wxSplitterEvent, wxAuiManagerEvent, wxEvtHandler, wxContextMenuEvent, wxLayoutAlgorithm, wxGridCellBoolEditor, wxMultiChoiceDialog, wxOverlay, wxShowEvent, wxAuiDockArt, wxDropFilesEvent, wxSizeEvent, wxComboBox, wxCloseEvent, wxBookCtrlBase, wxSashWindow, wxBitmapDataObject, wxXmlResource, wxGridCellChoiceEditor, wxListbook, wxImageList, wxToolTip, wxSlider, wxSizer, wxGBSizerItem, wxPen, wxAuiNotebook, wxGLCanvas, wxPanel, wxAuiNotebookEvent, wxStaticBoxSizer, wxUpdateUIEvent, wxColourData, wxIcon, wxColourPickerEvent, wxBitmapButton, wxImage, wxPageSetupDialogData, wxSplashScreen, wxMemoryDC, wxGridCellStringRenderer, wxPopupTransientWindow, wxGCDC, wxAcceleratorEntry, wxRadioButton, wxPickerBase, wxAcceleratorTable, wxTextDataObject, wxDC, wxCaret, wxPasswordEntryDialog, wxMenuEvent, wxMDIParentFrame, wxPreviewControlBar, wxHelpEvent, wxSpinButton, wxGenericDirCtrl, wxToolbook, wxFont, wxDatePickerCtrl, wxSystemSettings, wxWindow, wxMouseCaptureChangedEvent, wxTreeEvent, wxGauge, wxSingleChoiceDialog, wxFontPickerEvent, wxPrinter, wxTaskBarIcon, wxAuiTabArt, wxIconBundle, wxClientDC, wxPageSetupDialog, wxDirDialog, wxGridEvent, wx_misc, wxPreviewCanvas, wxTextAttr, wxScrollWinEvent, glu, gl,
    %% Handcrafted modules
    wx,
    wx_object,
    wxe_master,
    wxe_server,
    wxe_util
   ]},
  {registered, []},
  {applications, [stdlib, kernel]},
  {env, []},
  {runtime_dependencies, ["stdlib-5.0","kernel-8.0","erts-12.0"]}
 ]}.
