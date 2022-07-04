###########################################################################
## Python code generated with wxFormBuilder (version 3.10.0-4761b0c)
## http://www.wxformbuilder.org/
##
## PLEASE DO *NOT* EDIT THIS FILE!
###########################################################################

import wx
import wx.xrc
import wx.dataview as dv
#from tokenize import tokenize, untokenize, NUMBER, STRING, NAME, OP
import os
from wx_evci_mod import TokNperM2, XML_reader,TokNperM3,ToMeter, materials,sections

g=9.806
strutype=""
scaleS = 0.0
lineinput = []
ndf=0
fname = ' '
###########################################################################
## Class EVCI_Form
###########################################################################

class EVCI_Form ( wx.Frame ):
    
    

	def __init__( self, parent ):
		wx.Frame.__init__ ( self, parent, id = wx.ID_ANY, title = u"Structural Analysis & Design ", pos = wx.DefaultPosition, size = wx.Size( 1100,700 ), style = wx.CAPTION|wx.CLOSE_BOX|wx.DEFAULT_FRAME_STYLE|wx.TAB_TRAVERSAL )

		self.SetSizeHints( wx.DefaultSize, wx.DefaultSize )
		self.SetForegroundColour( wx.SystemSettings.GetColour( wx.SYS_COLOUR_BTNHIGHLIGHT ) )
		self.SetBackgroundColour( wx.SystemSettings.GetColour( wx.SYS_COLOUR_INACTIVECAPTION ) )

		bSizer3 = wx.BoxSizer( wx.VERTICAL )

		self.m_panel1 = wx.Panel( self, wx.ID_ANY, wx.DefaultPosition, wx.Size( -1,-1 ), wx.BORDER_SIMPLE|wx.TAB_TRAVERSAL )
		self.m_panel1.SetBackgroundColour( wx.SystemSettings.GetColour( wx.SYS_COLOUR_WINDOW ) )

		bSizer2 = wx.BoxSizer( wx.HORIZONTAL )

		self.m_toolBar1 = wx.ToolBar( self.m_panel1, wx.ID_ANY, wx.DefaultPosition, wx.DefaultSize, 0|wx.BORDER_RAISED )
		self.m_toolBar1.SetToolSeparation( 10 )
		self.m_toolBar1.SetBackgroundColour( wx.SystemSettings.GetColour( wx.SYS_COLOUR_ACTIVEBORDER ) )

		self.m_bOpenFile = wx.BitmapButton( self.m_toolBar1, wx.ID_ANY, wx.NullBitmap, wx.DefaultPosition, wx.Size( 40,30 ), wx.BU_AUTODRAW|0 )

		#self.m_bOpenFile.SetDefault()
		#self.m_bOpenFile.SetLabelMarkup( u"Open File" )
		self.m_bOpenFile.SetBitmap( wx.ArtProvider.GetBitmap( wx.ART_FILE_OPEN, wx.ART_BUTTON ) )
		self.m_bOpenFile.SetBitmapCurrent( wx.ArtProvider.GetBitmap( wx.ART_GO_FORWARD, wx.ART_MENU ) )
		self.m_bOpenFile.SetToolTip( u"Open File" )
		self.m_bOpenFile.SetMinSize( wx.Size( 40,40 ) )

		self.m_toolBar1.AddControl( self.m_bOpenFile )
		self.m_toolBar1.AddSeparator()

		self.m_bSaveFile = wx.BitmapButton( self.m_toolBar1, wx.ID_ANY, wx.NullBitmap, wx.DefaultPosition, wx.Size( 40,30 ), wx.BU_AUTODRAW|0 )

		self.m_bSaveFile.SetBitmap( wx.ArtProvider.GetBitmap( wx.ART_FILE_SAVE, wx.ART_BUTTON ) )
		self.m_bSaveFile.SetToolTip( u"Save Input File" )
		self.m_bSaveFile.SetMinSize( wx.Size( 40,40 ) )

		self.m_toolBar1.AddControl( self.m_bSaveFile )
		self.m_toolBar1.AddSeparator()

		self.m_bRunSolver = wx.BitmapButton( self.m_toolBar1, wx.ID_ANY, wx.NullBitmap, wx.DefaultPosition, wx.Size( 40,30 ), wx.BU_AUTODRAW|0 )

		self.m_bRunSolver.SetBitmap( wx.ArtProvider.GetBitmap( wx.ART_REDO, wx.ART_BUTTON ) )
		self.m_bRunSolver.SetToolTip( u"Run Solver" )
		self.m_bRunSolver.SetMinSize( wx.Size( 40,40 ) )

		self.m_toolBar1.AddControl( self.m_bRunSolver )
		self.m_toolBar1.AddSeparator()

		self.m_bHelp = wx.BitmapButton( self.m_toolBar1, wx.ID_ANY, wx.NullBitmap, wx.DefaultPosition, wx.Size( 40,30 ), wx.BU_AUTODRAW|0 )

		self.m_bHelp.SetBitmap( wx.ArtProvider.GetBitmap( wx.ART_HELP_BOOK, wx.ART_BUTTON ) )
		self.m_bHelp.SetToolTip( u"Help" )
		self.m_bHelp.SetMinSize( wx.Size( 40,40 ) )

		self.m_toolBar1.AddControl( self.m_bHelp )
		self.m_toolBar1.AddSeparator()

		self.m_bExit = wx.BitmapButton( self.m_toolBar1, wx.ID_ANY, wx.NullBitmap, wx.DefaultPosition, wx.Size( 40,30 ), wx.BU_AUTODRAW|0 )
		self.m_bExit.SetBitmap( wx.ArtProvider.GetBitmap( wx.ART_QUIT, wx.ART_BUTTON ) )
		self.m_bExit.SetToolTip( u"Exit Program" )
		self.m_bExit.SetMaxSize( wx.Size( 40,40 ) )

		self.m_toolBar1.AddControl( self.m_bExit )
		self.m_toolBar1.Realize()

		bSizer2.Add( self.m_toolBar1, 1, wx.EXPAND, 0 )


		self.m_panel1.SetSizer( bSizer2 )
		self.m_panel1.Layout()
		bSizer2.Fit( self.m_panel1 )
		bSizer3.Add( self.m_panel1, 0, wx.EXPAND, 0 )

		self.m_panel2 = wx.Panel( self, wx.ID_ANY, wx.DefaultPosition, wx.DefaultSize, wx.BORDER_RAISED|wx.TAB_TRAVERSAL )
		bSizer7 = wx.BoxSizer( wx.HORIZONTAL )

		self.m_dataViewTreeCtrl3 = wx.dataview.DataViewTreeCtrl( self.m_panel2, wx.ID_ANY, wx.DefaultPosition, wx.DefaultSize, 0|wx.HSCROLL|wx.VSCROLL )
		self.m_dataViewTreeCtrl3.SetToolTip( u"Open File" )

		bSizer7.Add( self.m_dataViewTreeCtrl3, 1, wx.EXPAND, 0 )

		self.m_textfilein = wx.TextCtrl( self.m_panel2, wx.ID_ANY, wx.EmptyString, wx.DefaultPosition, wx.DefaultSize, wx.HSCROLL|wx.TE_MULTILINE|wx.HSCROLL|wx.VSCROLL )
		bSizer7.Add( self.m_textfilein, 2, wx.EXPAND, 2 )

		self.m_textfileout = wx.TextCtrl( self.m_panel2, wx.ID_ANY, wx.EmptyString, wx.DefaultPosition, wx.DefaultSize, wx.HSCROLL|wx.TE_MULTILINE|wx.HSCROLL|wx.VSCROLL )
		bSizer7.Add( self.m_textfileout, 2, wx.EXPAND, 2 )


		self.m_panel2.SetSizer( bSizer7 )
		self.m_panel2.Layout()
		bSizer7.Fit( self.m_panel2 )
		bSizer3.Add( self.m_panel2, 2, wx.EXPAND, 0 )

		bSizer4 = wx.BoxSizer( wx.VERTICAL )

		self.m_textlog = wx.TextCtrl( self, wx.ID_ANY, wx.EmptyString, wx.DefaultPosition, wx.DefaultSize, wx.HSCROLL|wx.TE_MULTILINE )
		bSizer4.Add( self.m_textlog, 1, wx.ALL|wx.EXPAND, 2 )


		bSizer3.Add( bSizer4, 1, wx.EXPAND, 5 )


		self.SetSizer( bSizer3 )
		self.Layout()
		self.m_menubar1 = wx.MenuBar( 0 )
		self.m_menubar1.SetForegroundColour( wx.SystemSettings.GetColour( wx.SYS_COLOUR_BTNTEXT ) )
		self.m_menubar1.SetBackgroundColour( wx.SystemSettings.GetColour( wx.SYS_COLOUR_ACTIVEBORDER ) )
# Menu
		self.File_mnu = wx.Menu()
		self.m_Open = wx.MenuItem( self.File_mnu, wx.ID_ANY, u"Open", u"Open Input File", wx.ITEM_NORMAL )
		self.m_Open.SetBitmap( wx.ArtProvider.GetBitmap( wx.ART_FILE_OPEN, wx.ART_MENU ) )
		self.File_mnu.Append( self.m_Open )

		self.m_Save_input = wx.MenuItem( self.File_mnu, wx.ID_ANY, u"Save Input", wx.EmptyString, wx.ITEM_NORMAL )
		self.m_Save_input.SetBitmap( wx.ArtProvider.GetBitmap( wx.ART_FILE_SAVE, wx.ART_MENU ) )
		self.File_mnu.Append( self.m_Save_input )

		self.m_Save_results = wx.MenuItem( self.File_mnu, wx.ID_ANY, u"Save Results", wx.EmptyString, wx.ITEM_NORMAL )
		self.m_Save_results.SetBitmap( wx.ArtProvider.GetBitmap( wx.ART_FILE_SAVE_AS, wx.ART_MENU ) )
		self.File_mnu.Append( self.m_Save_results )

		self.File_mnu.AppendSeparator()

		self.m_Exit = wx.MenuItem( self.File_mnu, wx.ID_ANY, u"Exit", wx.EmptyString, wx.ITEM_NORMAL )
		self.m_Exit.SetBitmap( wx.ArtProvider.GetBitmap( wx.ART_CLOSE, wx.ART_MENU ) )
		self.File_mnu.Append( self.m_Exit )

		self.m_menubar1.Append( self.File_mnu, u"File" )

		self.Edit_mnu = wx.Menu()
		self.m_cut = wx.MenuItem( self.Edit_mnu, wx.ID_ANY, u"Cut", wx.EmptyString, wx.ITEM_NORMAL )
		self.m_cut.SetBitmap( wx.ArtProvider.GetBitmap( wx.ART_CUT, wx.ART_MENU ) )
		self.Edit_mnu.Append( self.m_cut )

		self.m_delete = wx.MenuItem( self.Edit_mnu, wx.ID_ANY, u"Delete", wx.EmptyString, wx.ITEM_NORMAL )
		self.m_delete.SetBitmap( wx.ArtProvider.GetBitmap( wx.ART_DELETE, wx.ART_MENU ) )
		self.Edit_mnu.Append( self.m_delete )

		self.m_copy = wx.MenuItem( self.Edit_mnu, wx.ID_ANY, u"Copy", wx.EmptyString, wx.ITEM_NORMAL )
		self.m_copy.SetBitmap( wx.ArtProvider.GetBitmap( wx.ART_COPY, wx.ART_MENU ) )
		self.Edit_mnu.Append( self.m_copy )

		self.m_paste = wx.MenuItem( self.Edit_mnu, wx.ID_ANY, u"Paste", wx.EmptyString, wx.ITEM_NORMAL )
		self.m_paste.SetBitmap( wx.ArtProvider.GetBitmap( wx.ART_PASTE, wx.ART_MENU ) )
		self.Edit_mnu.Append( self.m_paste )

		self.m_menubar1.Append( self.Edit_mnu, u"Edit" )

		self.Display_mnu = wx.Menu()
		self.m_displacements = wx.MenuItem( self.Display_mnu, wx.ID_ANY, u"Displacements", wx.EmptyString, wx.ITEM_NORMAL )
		self.m_displacements.SetBitmap( wx.ArtProvider.GetBitmap( wx.ART_REDO, wx.ART_MENU ) )
		self.Display_mnu.Append( self.m_displacements )

		self.m_display_reactions = wx.MenuItem( self.Display_mnu, wx.ID_ANY, u"Reactions", wx.EmptyString, wx.ITEM_NORMAL )
		self.m_display_reactions.SetBitmap( wx.ArtProvider.GetBitmap( wx.ART_GOTO_LAST, wx.ART_MENU ) )
		self.Display_mnu.Append( self.m_display_reactions )

		self.m_display_intforces = wx.MenuItem( self.Display_mnu, wx.ID_ANY, u"Internal Forces", wx.EmptyString, wx.ITEM_NORMAL )
		self.m_display_intforces.SetBitmap( wx.ArtProvider.GetBitmap( wx.ART_GO_FORWARD, wx.ART_MENU ) )
		self.Display_mnu.Append( self.m_display_intforces )

		self.Display_mnu.AppendSeparator()

		self.m_clear = wx.MenuItem( self.Display_mnu, wx.ID_ANY, u"Clear", wx.EmptyString, wx.ITEM_NORMAL )
		self.m_clear.SetBitmap( wx.ArtProvider.GetBitmap( wx.ART_DELETE, wx.ART_MENU ) )
		self.Display_mnu.Append( self.m_clear )

		self.m_menubar1.Append( self.Display_mnu, u"Display" )

		self.Codes_mnu = wx.Menu()
		self.AISC_mnu = wx.Menu()
		self.m_asd = wx.MenuItem( self.AISC_mnu, wx.ID_ANY, u"ASD", wx.EmptyString, wx.ITEM_NORMAL )
		self.m_asd.SetBitmap( wx.ArtProvider.GetBitmap( wx.ART_HELP_SIDE_PANEL, wx.ART_MENU ) )
		self.AISC_mnu.Append( self.m_asd )

		self.m_lrfd = wx.MenuItem( self.AISC_mnu, wx.ID_ANY, u"LRFD", wx.EmptyString, wx.ITEM_NORMAL )

		#self.m_lrfd.SetBitmap( wx.Bitmap( u"C:\\Users\\edval\\Dropbox\\wxFB\\resources\\exefile.xpm", wx.BITMAP_TYPE_ANY ) )
		self.m_lrfd.SetBitmap(wx.Bitmap(u"/mnt/c/Users/edval/Dropbox/wxFB/resources/exefile.xpm", wx.BITMAP_TYPE_ANY) )
		self.AISC_mnu.Append( self.m_lrfd )

		self.Codes_mnu.AppendSubMenu( self.AISC_mnu, u"AISC" )

		self.m_api = wx.Menu()
		self.m_2rd = wx.MenuItem( self.m_api, wx.ID_ANY, u"2RD", wx.EmptyString, wx.ITEM_NORMAL )
		#self.m_2rd.SetBitmap( wx.Bitmap( u"C:\\Users\\edval\\Dropbox\\wxFB\\resources\\state2.xpm", wx.BITMAP_TYPE_ANY ) )
		self.m_2rd.SetBitmap( wx.Bitmap( u"/mnt/c/Users/edval/Dropbox/wxFB/resources/state2.xpm", wx.BITMAP_TYPE_ANY ) )  
		self.m_api.Append( self.m_2rd )

		self.Codes_mnu.AppendSubMenu( self.m_api, u"API" )

		self.m_menubar1.Append( self.Codes_mnu, u"Codes" )

		self.SetMenuBar( self.m_menubar1 )

		#self.m_statusBar1 = self.CreateStatusBar( 3, wx.STB_SIZEGRIP, wx.ID_ANY )
		self.m_statusBar1 = self.CreateStatusBar(3, wx.STB_SIZEGRIP, wx.ID_ANY)
		self.m_statusBar1.SetBackgroundColour( wx.SystemSettings.GetColour( wx.SYS_COLOUR_HIGHLIGHTTEXT ) )


		self.Centre( wx.BOTH )

		# Connect Events
		self.m_bOpenFile.Bind( wx.EVT_BUTTON, self.OpenFile_click )
		self.m_bSaveFile.Bind( wx.EVT_BUTTON, self.SaveFile_click )
		self.m_bRunSolver.Bind( wx.EVT_BUTTON, self.RunSolver_click )
		self.m_bHelp.Bind( wx.EVT_BUTTON, self.Help_click )
		self.m_bExit.Bind( wx.EVT_BUTTON, self.Exit_click )
		self.m_textfilein.Bind( wx.EVT_TEXT, self.OnText_changed )
		self.Bind( wx.EVT_MENU, self.wxmnu_Open_click, id = self.m_Open.GetId() )
		self.Bind( wx.EVT_MENU, self.wxmnu_inputSave_Click, id = self.m_Save_input.GetId() )
		self.Bind( wx.EVT_MENU, self.wxmnu_resultsSave_Click, id = self.m_Save_results.GetId() )
		self.Bind( wx.EVT_MENU, self.wxmmnu_Exit_Click, id = self.m_Exit.GetId() )
		self.Bind( wx.EVT_MENU, self.wxmnu_cut, id = self.m_cut.GetId() )
		self.Bind( wx.EVT_MENU, self.wxmnu_delete, id = self.m_delete.GetId() )
		self.Bind( wx.EVT_MENU, self.wxmnu_copy, id = self.m_copy.GetId() )
		self.Bind( wx.EVT_MENU, self.wxmnu_paste, id = self.m_paste.GetId() )
		self.Bind( wx.EVT_MENU, self.wxmnu_displacements, id = self.m_displacements.GetId() )
		self.Bind( wx.EVT_MENU, self.wxmnu_display_reactions, id = self.m_display_reactions.GetId() )
		self.Bind( wx.EVT_MENU, self.wxmnu_display_intforces, id = self.m_display_intforces.GetId() )
		self.Bind( wx.EVT_MENU, self.wxmnu_clear, id = self.m_clear.GetId() )
		self.Bind( wx.EVT_MENU, self.wxmnu_asd, id = self.m_asd.GetId() )
		self.Bind( wx.EVT_MENU, self.wxmnu_lrfd, id = self.m_lrfd.GetId() )
		self.Bind( wx.EVT_MENU, self.wxmnu_2rd, id = self.m_2rd.GetId() )

		

	def __del__( self ):
		pass


	# Virtual event handlers, override them in your derived class
	def OpenFile_click( self, e ):
		#event.Skip()
		global fname
		wildcard = "XML Files (*.xml)|*.xml"
		dlg = wx.FileDialog(self, "Choose a file", os.getcwd(),
		                    "", wildcard, wx.FD_OPEN)
		if dlg.ShowModal() == wx.ID_OK:
			f = open(dlg.GetPath(), 'r')
			with f:
				data = f.read()
				self.m_textfilein.SetValue(data)
			fname = f.name
			self.m_statusBar1.PushStatusText(fname)
			#print(fname)
			XML_reader(fname)
			print(materials)
		elif dlg.ShowModal() == wx.ID_CANCEL:
			wx.MessageBox("No file selected","Try again: select input file",wx.ICON_QUESTION |wx.OK)
			return
		dlg.Destroy()
		#return fname

	def SaveFile_click( self, event ):
		event.Skip()

	def RunSolver_click( self, event ):
		event.Skip()

	def Help_click( self, event ):
		event.Skip()

	def Exit_click( self, e ):
		#event.Skip()
		if wx.MessageBox("Quit Program?","Please, confirm", wx.ICON_QUESTION | wx.YES_NO,self) == wx.NO:
			return
		self.Close()
   		#wx.Exit()

	def OnText_changed( self, event ):
		event.Skip()

	def wxmnu_Open_click( self, e ):
		#event.Skip()
		wildcard = "XML Files (*.xml)|*.xml"
		dlg = wx.FileDialog(self, "Choose a file", os.getcwd(), "", wildcard, wx.FD_OPEN)
		if dlg.ShowModal() == wx.ID_OK: 
			f = open(dlg.GetPath(), 'r') 
			global fname
			with f: 
				data = f.read() 
				self.m_textfilein.SetValue(data)       
			fname=f.name 
			self.m_statusBar1.title=fname
			self.m_statusBar1 = fname
			#print(fname)
			XML_reader(fname)
			print(materials)
		elif dlg.ShowModal() == wx.ID_CANCEL:
			wx.MessageBox("No file selected","Try again: select input file", wx.ICON_QUESTION | wx.OK)
			return
		dlg.Destroy()
		#return fname         

	def wxmnu_inputSave_Click( self, event ):
		event.Skip()

	def wxmnu_resultsSave_Click( self, event ):
		event.Skip()

	def wxmmnu_Exit_Click( self, e ):
		#event.Skip()
		if wx.MessageBox("Quit Program?", "Please, confirm", wx.ICON_QUESTION | wx.YES_NO, self) == wx.NO:
			return
		self.Close()

	def wxmnu_cut( self, event ):
		event.Skip()

	def wxmnu_delete( self, event ):
		event.Skip()

	def wxmnu_copy( self, event ):
		event.Skip()

	def wxmnu_paste( self, event ):
		event.Skip()

	def wxmnu_displacements( self, event ):
		event.Skip()

	def wxmnu_display_reactions( self, event ):
		event.Skip()

	def wxmnu_display_intforces( self, event ):
		event.Skip()

	def wxmnu_clear( self, event ):
		event.Skip()

	def wxmnu_asd( self, event ):
		event.Skip()

	def wxmnu_lrfd( self, event ):
		event.Skip()

	def wxmnu_2rd( self, event ):
		event.Skip()


	# def TokNperM2(unitF):
	# 	if unitF == "kN/m2":
	# 		tonewton = 1.0
	# 	elif unitF == "lb/sqf":
	# 		tonewton = 0.047880258888889
	# 	elif unitF == "ksi":
	# 		tonewton = 6894.757280000001
	# 	elif unitF == "kgf/m2":
	# 		tonewton = g
	# 	elif unitF == "ton/sqf":
	# 		tonewton = 107.25177991111
	# 	elif unitF == "psi":
	# 		tonewton = 6.89475728
	# 	else:
	# 		tonewton = 1.0


	# def TokNperM3(unitF):
	# 	if unitF == "kN/m3":
	# 		tonewton = 1.0
	# 	elif unitF == "lbf/ft3":
	# 		tonewton = 0.1570865731
	# 	elif unitF == "kips/inch3":
	# 		tonewton = 271447.14116097
	# 	elif unitF == "kgf/m3":
	# 		tonewton = g/1000.0
	# 	elif unitF == "tonf/ft3":
	# 		tonewton = 314.1731461238
	# 	elif unitF == "t/m3":
	# 		tonewton = g
	# 	else:
	# 		tonewton = 1.0


	# def XML_reader(m_dataViewTreeCtrl3):
    # # start processing the XML file
	# 	doc = wx.XmlDocument(fname)
	# 	if not doc.Load(fname):
	# 		return False

	# 	strutype = doc.GetRoot().GetName()
	# 	if strutype == "Frame2D":	ndf = 3
	# 	elif strutype == "Frame3D":	ndf = 6
	# 	elif strutype == "Truss3D":	ndf = 3
	# 	elif strutype == "Truss2D":	ndf = 2
	# 	elif strutype == "Grid":  	ndf = 3
	# 	elif strutype == "Frame2D_8DOF": ndf = 4
	# 	else: 	ndf = 0
	# 	print(strutype)

    # #     if child.GetType() == wx.xml.XML_PI_NODE and child.GetName() == "target":

    # #         # process Process Instruction contents
    # #         pi = child.GetContent()

    #     # Other code here...
	# 	child = doc.GetRoot().GetChildren()
	# 	m_dataViewTreeCtrl3=dv.DataViewCtrl()
	# 	rootID=wx.DataViewModel()
	# 	m_dataViewTreeCtrl3.AssociateModel(rootID)
	# 	#rootId = m_dataViewTreeCtrl3.AppendContainer(wx.dataview.wxDataViewItem(0), strutype, 0)
	# 	while child:
	# 		tagname = child.GetName()
	# 		content = child.GetNodeContent()  # process text enclosed by tag1/tag1

	# 		if tagname == "title":
	# 			projName = content
	# 			#project = m_dataViewTreeCtrl3.AppendContainer(rootId,"Project Name",1)
	# 			#m_dataViewTreeCtrl3.AppendItem(project,projName,1)
	# 			# attrvalue1 = child.GetAttribute("attr1", "default-value")
	# 			#attrvalue2 = child.GetAttribute("attr2", "default-value")
	# 		elif tagname == "code":
	# 			# UnitS: stress unit ...
	# 			UnitS = child.GetAttribute("unitS", "kN/m2")
	# 			if UnitS == "default-value": 	scaleS = 1.0
	# 		else:     			scaleS =TokNperM2(UnitS)
	# 		tokens = tokenize.generate_tokens(content)
	# 		for token in tokens:
	# 			lineinput.append(token)
	# 		for x in lineinput:
	# 			if x == "AISC": Code = x
	# 			elif x == "fy": fyield = float(x)*scaleS
	# 		child = child.GetNext()
  
if __name__ == '__main__':
# When this module is run (not imported) then create the app, the
# frame, show it, and start the event loop.
    app = wx.App()

    frm = EVCI_Form(None)
    frm.Show()
    #window.Show(True)
    app.MainLoop()
