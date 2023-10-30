###########################################################################
## Python code generated with wxFormBuilder (version 3.10.0-4761b0c)
## http://www.wxformbuilder.org/
##
## PLEASE DO *NOT* EDIT THIS FILE!
###########################################################################


#from tokenize import tokenize, untokenize, NUMBER, STRING, NAME, OP
import os
import math
import numpy as np
import wx
import wx.dataview as dv
import wx.xrc
import matplotlib.pyplot as plt
import mpl_toolkits.mplot3d.art3d as plot3d
#from matplotlib.collections import LineCollection
#from mpl_toolkits.mplot3d import Axes3D
#from mpl_toolkits.mplot3d.art3d import Poly3DCollection,Line3DCollection
import pylsa 
from wx_evci_mod import StruMod as sm
#from wire3d_mod import Tkwireframe2D as tk2d
#import datetime
from datetime import datetime

###########################################################################
## Class EVCI_Form
###########################################################################

# class EVCI_Form ( wx.Frame,sm,tk2d ):

class EVCI_Form ( wx.Frame,sm ):

	def __init__( self, parent ):
		wx.Frame.__init__ ( self, parent, id = wx.ID_ANY, title = u"Structural Analysis & Design ", pos = wx.DefaultPosition, size = wx.Size( 1100,700 ), style = wx.CAPTION|wx.CLOSE_BOX|wx.DEFAULT_FRAME_STYLE|wx.TAB_TRAVERSAL )

	# def __init__(self):
	# 	super().__init__(parent=None, tile='Structural Analysis & Design', size=wx.Size(1100, 700))

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

		# self.m_dataViewTreeCtrl3 = wx.dataview.DataViewTreeCtrl( self.m_panel2, wx.ID_ANY, wx.DefaultPosition, wx.DefaultSize, 0|wx.HSCROLL|wx.VSCROLL )
		self.m_dataViewTreeCtrl3 = wx.TreeCtrl( self.m_panel2, wx.ID_ANY, wx.DefaultPosition, wx.DefaultSize, 0|wx.HSCROLL|wx.VSCROLL )  
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

		self.m_clear = wx.MenuItem( self.Display_mnu, wx.ID_ANY, u"Show Structure", wx.EmptyString, wx.ITEM_NORMAL )
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

		self.m_statusBar1 = self.CreateStatusBar( 3, wx.STB_SIZEGRIP, wx.ID_ANY )
		# self.CreateStatusBar(3, wx.STB_SIZEGRIP, wx.ID_ANY)
		#self.m_statusBar1 = wx.StatusBar(self, wx.STB_DEFAULT_STYLE, wx.WindowID)
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

	def show_dv(self):
		self.root=self.m_dataViewTreeCtrl3.AddRoot(sm.strutype)
		proj=self.m_dataViewTreeCtrl3.AppendItem(self.root,'Project')
		self.m_dataViewTreeCtrl3.AppendItem(proj,sm.exampletitle)
		code=self.m_dataViewTreeCtrl3.AppendItem(self.root,'Code')
		self.m_dataViewTreeCtrl3.AppendItem(code,sm.lines_code)
		mat=self.m_dataViewTreeCtrl3.AppendItem(self.root,'Material')
		for line in sm.lines_mat:
			self.m_dataViewTreeCtrl3.AppendItem(mat,line)
		sec=self.m_dataViewTreeCtrl3.AppendItem(self.root,'Sections')
		for line in sm.lines_sec:
			self.m_dataViewTreeCtrl3.AppendItem(sec,line)
		nodes=self.m_dataViewTreeCtrl3.AppendItem(self.root,'Nodes')
		for i in range(len(sm.lines_nodes)):
			self.m_dataViewTreeCtrl3.AppendItem(nodes,sm.lines_nodes[i])			
		elem=self.m_dataViewTreeCtrl3.AppendItem(self.root,'Elements')
		for line in sm.lines_elem:
			self.m_dataViewTreeCtrl3.AppendItem(elem,line)
		bnd=self.m_dataViewTreeCtrl3.AppendItem(self.root,'Boundary')
		for line in sm.lines_bnd:
			self.m_dataViewTreeCtrl3.AppendItem(bnd,line)
		loading=self.m_dataViewTreeCtrl3.AppendItem(self.root,'Loading')
		for line in sm.lines_loading:
			self.m_dataViewTreeCtrl3.AppendItem(loading,line)


		self.m_dataViewTreeCtrl3.Expand(self.root)
		self.Show()

	# Virtual event handlers, override them in your derived class
	def OpenFile_click( self, e ):
		sizes=[8,10,12,14]
		families={"FONTFAMILY_ROMAN:wx.FONTFAMILY_ROMAN"}
		#font1 = wx.Font(sizes[1], families, False, u'Consolas')
		#font2=wx.Font(10)
		#self.m_textfilein.SetFont(font1)
		fileout = open("output.txt", "a")
		wildcard = "XML Files (*.xml)|*.xml"
		dlg = wx.FileDialog(self, "Choose a file", os.getcwd(),"", wildcard, wx.FD_OPEN)
		if dlg.ShowModal() == wx.ID_OK:
			f = open(dlg.GetPath(), 'r')
			with f:
				data = f.read()
				self.m_textfilein.SetValue(data)
			fname = f.name
			self.SetStatusText(fname)
			self.m_statusBar1.SetStatusText(fname)
			sm.XML_reader(fname)
			fileout = open("output.txt", 'r')
			self.m_textlog.SetValue(fileout.read())
			fileout.close()
		elif dlg.ShowModal() == wx.ID_CANCEL:
			wx.MessageBox("No file selected","Try again: select input file",wx.ICON_QUESTION |wx.OK)
			return
		self.show_dv()
		dlg.Destroy()

	def SaveFile_click( self, event ):
		event.Skip()

	def RunSolver_click( self, event ):
		#print (pylsa.stru3d.__doc__)
		pylsa.stru3d.nn=sm.nn
		pylsa.stru3d.ne = sm.ne
		pylsa.stru3d.nbn = sm.nbn
		pylsa.stru3d.n = sm.n
		pylsa.stru3d.ms = sm.ms
		pylsa.stru3d.ndf = sm.ndf
		pylsa.stru3d.nne = sm.nne
		pylsa.stru3d.ndfel=sm.ndfel
		pylsa.stru3d.nlmem=sm.nlmem
		pylsa.stru3d.nlc=sm.nlc
		pylsa.stru3d.kiter=1
		pylsa.stru3d.slen=1
		pylsa.stru3d.kip=1
		pylsa.stru3d.strutype = sm.strutype
		pylsa.stru3d.exampletitle=sm.exampletitle

		xstring=np.array(sm.nodelist,dtype='c').T
		ystring = np.array(sm.elemlist, dtype='c').T

		pylsa.stru3d.nodebytes=xstring
		pylsa.stru3d.elembytes = ystring
		pylsa.stru3d.elem_prop = sm.elem_prop_arr
		pylsa.stru3d.sec_table=sm.sections_arr
		pylsa.stru3d.mat_table=sm.mat_table
		import datetime
		pylsa.stru3d.tk=np.zeros((sm.n,sm.ms))
		pylsa.stru3d.fem_dload=np.zeros((sm.ne,sm.ndfel))
		pylsa.stru3d.mfem_load=np.zeros((sm.ne*sm.nlc,sm.ndfel))

		pylsa.stru3d.intforc=np.zeros(sm.ne*sm.nlc*sm.ndfel)

		pylsa.stru3d.al=sm.al
		pylsa.stru3d.reac=sm.reac
		pylsa.stru3d.mfem_param=sm.mfem_param
		pylsa.stru3d.ib=sm.ib
		if(sm.nlmem>0): 
			pylsa.stru3d.mfemgen()
		dt1=datetime.datetime.now()
		pylsa.stru3d.k_assem()
		self.m_textlog.AppendText("Assembly of Stiffness Matrix ...Completed\n")
		pylsa.stru3d.boundgen()
		pylsa.stru3d.bgaussgen()
		pylsa.stru3d.forcegen()
		pylsa.stru3d.outptgen()
		dt2=datetime.datetime.now()
		dt=dt2-dt1
		txt="Solution of Equations Solver ..... Completed " + ' Elapsed Time: '+str(float(dt.seconds)) + ' seconds\n'
		self.m_textlog.AppendText(txt)
		f = open("fortran_out.txt", "r")
		with f:
			data = f.read()
			self.m_textfileout.SetValue(data)
		f.close()
		self.outputgen()

	def Help_click( self, event ):
		event.Skip()

	def Exit_click( self, e ):
		if wx.MessageBox("Quit Program?","Please, confirm", wx.ICON_QUESTION | wx.YES_NO,self) == wx.NO:
			return
		self.Close()
   		#wx.Exit()

	def OnText_changed( self, event ):
		event.Skip()

	def wxmnu_Open_click( self, e ):   

		wildcard = "XML Files (*.xml)|*.xml"
		dlg = wx.FileDialog(self, "Choose a file", os.getcwd(), "", wildcard, wx.FD_OPEN)
		if dlg.ShowModal() == wx.ID_OK: 
			f = open(dlg.GetPath(), 'r')
			with f: 
				data = f.read() 
				self.m_textfilein.SetValue(data)       
			fname=f.name
			index=fname.index('.')
			#self.SetStatusText(self,fname)
			self.m_statusBar1.SetStatusText(fname)
			sm.XML_reader(fname)
			fileout = open("output.txt", 'r')
			self.m_textlog.SetValue(fileout.read())
			fileout.close()
			self.show_dv()

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
		colors=['blue','green','purple','red','yellow']
		for i in range(sm.nsec):
			sm.sec_color.append(colors[i])
		fig = plt.figure()
		fig.set_size_inches(8.5, 8.5)
		ax = fig.add_subplot(111, projection='3d')# , aspect='equal')
		#ax=plt.axes(projection='3d')
		
		if(sm.strutype=='Frame2D   '):
			ax.view_init(elev=90,azim=-90,roll=0)
		elif(sm.strutype=='Truss2D   '):
			ax.view_init(elev=90,azim=-90,roll=0)
		else:
			ax.view_init(azim=45)
		ax.set_xlabel('X Coordinate (m)')
		ax.set_ylabel('Y Coordinate (m)')
		ax.set_zlabel('Z Coordinate (m)')
		#ax.set_box_aspect([1.0,1.0,1.0])
		#ax.set_aspect('auto')
		ax.set_title(sm.exampletitle)
		x=np.reshape(sm.x,newshape=sm.nn)
		y = np.reshape(sm.y, newshape=sm.nn)
		z = np.reshape(sm.z, newshape=sm.nn)
		#print('{0} {1} {2}'.format(x,y,z))
		#vertices=[list(zip(x,y,z))]
		#poly=Poly3DCollection(vertices,alpha=0.8)
		#plt.show()
		xmax=max(x)+5
		ymax=max(y)+5
		ax.scatter3D(x, y, z,color='red', marker='s') #, c=np.array(zz), cmap='Greens') #,rstride=10, cstride=10)
		for i in range(sm.nn):
			ax.text(x[i]+0.5,y[i],z[i],sm.nodelist[i],color='blue')
		ymax=max(y)
		for i in range(sm.nsec):
			ax.text(0.5,ymax+5-i*2,0,sm.sec_label[i])
		#print(sm.elem_prop)

		ax.set_xlim(0,xmax)
		ax.set_ylim(0,ymax)
		ax.set_aspect('equal',None)
		for i in range(sm.ne):
			inc1=int(sm.elem_proper[i][0])
			inc2 = int(sm.elem_proper[i][1])
			#print('{0} {1} {2}'.format(i,inc1,inc2))
			xs=x[inc1-1],x[inc2-1]
			ys=y[inc1-1],y[inc2-1]
			zs = z[inc1-1], z[inc2-1]
			index=int(sm.elem_prop_arr[i,2]-1)
			# line = plot3d.art3d.Line3D(xs, ys, zs)
			line = plot3d.Line3D(xs, ys, zs,color=sm.sec_color[index])
			ax.add_line(line)
		#line3d=[list(zip(lines))]
		#poly1=Line3D(line3d)
		#ax.plot(lines)
		#ax.plot(xs,ys,zs,'-b')
		plt.show()


	def wxmnu_asd( self, event ):
		event.Skip()

	def wxmnu_lrfd( self, event ):
		event.Skip()

	def wxmnu_2rd( self, event ):
		event.Skip()
	def AISC_360_16_ASD(self):
		'''This function calculates the unity ratio and Pr/Pc for each element in a 3D truss or 
		frame structure, based on the ANSI/AISC Code 360-16 (ASD/LRFD) standard. 
		The function loops over each load case and each element, and calculates the forces and moments 
		for each element based on its material and section properties. It then calculates the 
		unity ratio and Pr/Pc for each element, and writes the results to an output file.'''
		# Initialize variables
		''' The function uses several variables to store intermediate results, such as r, area, rmomi, 
		rmomj, rmom, sx, sy, rx, ry, Pr, slendy, fb, cc, Fe, uratio, blngth, E, cc1, Fcr, Pn, Pc, 
		alpha, zx, zy, Mnx, Mrx, klc, n1, n2, k1, k2, and eqtn. These variables are used to simplify 
		the calculation of the unity ratio and Pr/Pc for each element.'''
		r = 0; 	area = 0; 	rmomi = 0; 	rmomj = 0; 	rmom = 0.0; sx = 0; sy = 0;	rx = 0; ry = 0
		Pr = 0.0; slendy = 0.0; fb = 0; cc = 0; Fe = 0; uratio = 0; blngth = 0; E = 0; cc1 = 0
		Fcr = 0; Pn = 0; Pc = 0; alpha = 1.6; zx = 0
		zy = 0; Mnx = 0.0;  Mrx = 0.0;  klc = 0;  n1 = 0;  n2 = 0;  k1 = 0 ;  k2 = 0 ; eqtn = ""

		# Open output file and write header
		with open(pylsa.fileout, "a") as outfile:
			outfile.write("ANSI/AISC Code 360-16 (ASD/LRFD) - July 7, 2016 - Revised June 2019\n")

			# Loop over load cases
			for klc in range(1, pylsa.nlc + 1):
				outfile.write("Load Case: " + str(klc) + "\n")
				outfile.write("{0:10} {1:10} {2:10} {3:10}\n".format("Element", "Unity Ratio", "Equation", "Pr/Pc"))

				# Loop over elements
				for nel in range(1, sm.ne + 1):
					n1 = pylsa.wxelement[nel - 1].inc1
					n2 = pylsa.wxelement[nel - 1].inc2
					k1 = sm.ndfel * (nel - 1) + sm.ne * pylsa.ndfel * (klc - 1)
					k2 = k1 + 2
					blngth = pylsa.wxelement[nel - 1].d
					imat = pylsa.wxelement[nel - 1].mat_id
					E = pylsa.wxmaterial[imat].Ematerial
					isec = pylsa.wxelement[nel - 1].sec_id
					area = pylsa.wxsection[isec].A
					sx = pylsa.wxsection[isec].Sx
					sy = pylsa.wxsection[isec].Sy
					rx = pylsa.wxsection[isec].rx
					ry = pylsa.wxsection[isec].ry
					zx = pylsa.wxsection[isec].Zx
					zy = pylsa.wxsection[isec].Zy

					# Calculate axial and bending stressess
					if sm.strutype == "Frame2D":
						Pr = pylsa.intforc[k1]
						rmomi = pylsa.intforc[k2]
						k1 = k2 + 1
						k2 = k1 + 2
						rmomj = pylsa.intforc[k2]
						r = min(rx, ry)
						rmom = max(abs(rmomi), abs(rmomj))
						Mnx = sm.fyield * zx / alpha
						Mrx = rmom
						fx = rmom / sx
						faw = Pr / area
						slendy = blngth / r
						fy = 0.0
					elif sm.strutype == "Frame3D":
						Mny = sm.fyield * zy / alpha
						fx = rmom / sx
						fy = rmom / sy
						faw = Pr / area
						slendy = blngth / r

					# Calculate other parameters
					fb = 0.66 * sm.fyield
					if Pr > 0.0:
						cc = 4.71 * math.sqrt(E / sm.fyield)
						Fe = 5.149359 * E / (slendy ** 2)
						cc1 = sm.fyield / Fe
						if slendy <= cc or cc1 <= 2.25:
							Fcr = sm.fyield * 0.658 ** cc1
						else:
							Fcr = 0.877 * Fe
						Pn = Fcr * area
						Pc = Pn / alpha
						if Pr / Pc < 0.2:
							uratio = Pr / (2 * Pc) + abs(Mrx / Mnx)
							eqtn = "H1-1b"
						else:
							uratio = Pr / Pc + abs(Mrx / Mnx) * 8 / 9
							eqtn = "H1-1a"
					else:
						Pn = sm.fyield * area
						Pc = Pn / alpha
						uratio = abs(Pr / Pc) + abs(Mrx / Mnx)
						eqtn = "H1-2 (Flexure and Tension)"

					# Write results to output file
					outfile.write("{0:10} {1:10} {2:10} {3:10}\n".format(pylsa.wxelement[nel - 1].memberid, "{0:0.000}".format(uratio), eqtn, "{0:0.000}".format(Pr / Pc)))

			outfile.write("\n")

	def header(self):
		self.m_textfileout.write(" " * 20 + sm.PNAME + "\n")
		self.m_textfileout.write(sm.exampletitle + "\n")
		self.m_textfileout.write("-" * 80 + "\n")

	def time_now(self):
		now = datetime.now()
		tmpmonth = now.month
		tmpday = now.day
		tmpyear = now.year
		tmphour = now.hour
		tmpminute = now.minute
		tmpsecond = now.second
		tmphund = now.microsecond // 10000

		if tmphour > 12:
			mer = 'p'
			tmphour = tmphour - 12
		else:
			mer = 'a'

		self.m_textfileout.write("{0}/{1:02}/{2:04}\n".format(tmpmonth, tmpday, tmpyear))
		self.m_textfileout.write("{0:02}:{1:02}:{2:02} {3}m\n".format(tmphour, tmpminute, tmpsecond, mer))

# Define the function outptgen
	def outputgen(self):
		# Import the iso_fortran_env module
		#from iso_fortran_env import INTEGER
		
		# Declare variables
		k, k2, k1, klc = 0, 0, 0, 0
		i, j, nel, j1, l1, no, n1, kip, slen, kiter = 0, 0, 0, 0, 0, 0, 0, 0, 0,0
		dat = [["kN", "m"] for _ in range(4)]
		
		# Open the file "fortran_out.txt" for writing
		#with open("fortran_out.txt", "w") as fileout_unit:
			# Allocate memory for nodelist and elemlist
			#nodelist = nodebytes.copy()
			#elemlist = elembytes.copy()
			
			# Get the number of active units
		#nlc = pylsa.al.shape[1]
		#ndfel = sm.nne * sm.ndf
		
		# Set the units for the output data
		dat[0][0] = "kN"
		dat[0][1] = "m"
		
		# Call the header function
		self.header()
		
		# Write the number of iterations to the file if kiter > 0
		if kiter > 0:
			self.m_textfileout.write("Number of Iterations {}\n".format(kiter))
		
		# Write the nodal displacements for loading to the file
		sm.al = pylsa.stru3d.al
		sm.reac = pylsa.stru3d.reac

		# Write the nodal displacements for loading to the file
		for klc in range(1, sm.nlc):
			self.m_textfileout.write("Nodal Displacements for Loading {:3}\n".format(klc))
			self.m_textfileout.write("Active Units : {:8} {:8}\n".format(dat[slen][0], dat[kip][1]))
			if sm.strutype == "Frame3D":
				self.m_textfileout.write("Node       Dx         Dy         Dz        Rotx       Roty       Rotz\n")
			elif sm.strutype == "Frame2D":
				self.m_textfileout.write("Node               Dx         Dy         Rotz\n")
			for i in range(1, sm.nn):
				k1 = sm.ndf * (i-1) #+ 1
				k2 = k1 + sm.ndf - 1
				values = [pylsa.stru3d.al[j, klc-1] for j in range(k1, k2)]
				if len(values) < 6:
					values += [0] * (6 - len(values))  # Fill the rest with zeros
				self.m_textfileout.write("{:10} {:15.4g} {:15.4g} {:15.4g} {:15.4g} {:15.4g} {:15.4g}\n".format(
					sm.nodelist[i-1], *values
				))

		self.m_textfileout.write("-" * 80 + "\n")


		
		# Call the time_now function
		self.time_now()

		# Return from the function
		return

if __name__ == '__main__':
# When this module is run (not imported) then create the app, the
# frame, show it, and start the event loop.
    app = wx.App()

    frm = EVCI_Form(None)
    frm.Show()
    #window.Show(True)
    app.MainLoop()
