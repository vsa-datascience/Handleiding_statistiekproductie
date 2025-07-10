\NeedsTeXFormat{LaTeX2e}
\ProvidesClass{SV-Wat_is_een_statistiek}

\LoadClass{SVnote}




%%% disable hyphenation for transformation pdf to docx. delete package for final document.
\usepackage[none]{hyphenat}
\setmainfont{Cambria}
\setsansfont{Calibri}


\usepackage[most]{tcolorbox}
\usepackage{tabularray}
\usepackage{subcaption}
\usepackage{framed}

\DeclareTextFontCommand{\emph}{\color{maincol2}\em}


\newtcolorbox{info}[1][]{%
	enhanced,
	breakable,
	colback=palgray2,
	colframe=palgray4,
	coltext=textcol,
	boxrule=0.9pt,
	boxsep=5pt,
	arc=1pt,
	left=10mm,
	leftrule=0.8mm,
	overlay unbroken={
		\node[anchor=north west, xshift=4pt, yshift=-4pt] at (frame.north west){\includegraphics[width=5mm]{figures/denken}};
		},
	overlay first={
		\node[anchor=north west, xshift=4pt, yshift=-4pt] at (frame.north west){\includegraphics[width=5mm]{figures/denken}};
		},
	fontupper=\itshape\footnotesize,
	#1
	}








\newlist{richtlijnen}{enumerate}{1}
\setlist[richtlijnen]{
	label=\emph{Richtlijn \arabic*.},
	labelindent=0pt,
	labelwidth=10ex,
	labelsep=1ex,
	itemindent=!,
	leftmargin=3ex
	}

\newlist{gebruikersvoorwaarden}{itemize}{1}
\setlist[gebruikersvoorwaarden]{
	nosep,
	label={$\rightarrow$},
	labelindent=0pt,
	labelwidth=3ex,
	labelsep=1ex,
	itemindent=0pt,
	leftmargin=!
	}



\usepackage{hyphenat}
\hyphenation{sta-ti-stiek-reeks}





