\NeedsTeXFormat{LaTeX2e}
\ProvidesClass{VSAreport}

\LoadClass[12pt]{report}



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%% CLASS OPTIONS
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


\RequirePackage{pgfopts}
\RequirePackage{etoolbox}

\newbool{Wordversion}
\boolfalse{Wordversion}

\newcommand\@theme{VSA}

\pgfkeys{
  /VSAreport/.is family, 
  /VSAreport,
  Wordversion/.code = \booltrue{Wordversion},
  theme/.code = \renewcommand\@theme{#1},
  }
\ProcessPgfOptions{/VSAreport}

\ifdefstring{\@theme}{VSA}{}{
\ifdefstring{\@theme}{VSA-green}{}{
	\ClassError{VSAreport}{Invalid theme: '\@theme'}{Only 'VSA' and 'VSA-green' are supported. Please fix your theme option.}
	}}












%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%% SUPPORTING PACKAGES
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

\RequirePackage{etoolbox}
%\RequirePackage{environ}

\RequirePackage{graphicx}
\RequirePackage{tikz}
\RequirePackage{amsmath}
\RequirePackage{afterpage}
\RequirePackage[framemethod=TikZ]{mdframed}
\RequirePackage[most]{tcolorbox}
\RequirePackage{changepage}






%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%% LANGUAGE
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

\RequirePackage[dutch]{babel}

\RequirePackage{hyphenat}
\hyphenation{sta-ti-stiek-reeks}









%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%% PAGE LAY-OUT
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

\RequirePackage[]{geometry}
\geometry{
 	a4paper,
	top=36mm,
	bottom=42mm,
	inner=35mm,
	outer=35mm,
	bindingoffset=10mm,
	headheight=14pt,
	headsep=20pt,
	footskip=34pt,
	twoside
	}
	
\RequirePackage{parskip}
\widowpenalty=10000
\clubpenalty=10000
\sloppy

\setlength{\textfloatsep}{30pt plus 2.0pt minus 4.0pt}
\setlength{\floatsep}{30pt plus 2.0pt minus 2.0pt}   
\setlength{\intextsep}{30pt plus 2.0pt minus 2.0pt}  







%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%	 FONTS
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

\ifbool{Wordversion}{
	\def\fontsfile{VSAreport/fonts/CambriaCalibri}
	\hyphenpenalty=10000
	\exhyphenpenalty=10000
	}{
	\def\fontsfile{VSAreport/fonts/FlandersArt}
	}
\input{\fontsfile.tex}
\RequirePackage{anyfontsize}





%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%% COLORS
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

\input{VSAreport/colors/\@theme.tex}




%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%% HEADERS AND FOOTERS
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

\RequirePackage{fancyhdr}   
 
\def\setupfooters{
	\renewcommand{\footrulewidth}{0.4pt}
	\renewcommand{\footrule}{{\color{maincol2}\hrule width\headwidth height\footrulewidth \vskip-\footrulewidth}}	 
	\fancyfoot[LE]{\color{maincol2}\thepage}                   % Even pages: page number (left)
	\fancyfoot[RO]{\color{maincol2}\thepage}                   % Odd pages: page number (right)       
	}
	
\def\setupheaders{
	\renewcommand{\headrulewidth}{0.4pt}
	\renewcommand{\headrule}{{\color{maincol2}\hrule width\headwidth height\headrulewidth \vskip-\headrulewidth}}	
	\fancyhead[LE]{\color{maincol2}\nouppercase{\leftmark}}    % Even pages: book title (left)
	\fancyhead[RO]{\color{maincol2}\nouppercase{\rightmark}}   % Odd pages: chapter title (right)   
	}
	 
\pagestyle{fancy}  
\fancyhf{}
\renewcommand{\chaptermark}[1]{\markboth{\thechapter. #1}{}} 
\setupfooters
\setupheaders

\fancypagestyle{plain}{
	\fancyhf{}
	\renewcommand{\headrulewidth}{0pt}  
	\setupfooters
	}   









%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%% TITLE PAGE
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

\def\@shorttitle{}%
\def\@title{}
\renewcommand{\title}[2][]{%
	\def\@shorttitle{#1}%
	\def\@title{#2}%
	\ifx\@shorttitle\@empty
	\markboth{#2}{#2}%
	\else
	\markboth{#1}{#1}%
	\fi
	}
\def\@subtitle{}
\newcommand\subtitle[1]{\def\@subtitle{#1}}	
\def\@author{}
\renewcommand\author[1]{\def\@author{#1}}	
\def\@date{\today}
\renewcommand\date[1]{\def\@date{#1}}
\def\@titlepagebackground{}
\def\@titlepagebackgroundoptions{width=\paperwidth,height=\paperheight}
\newcommand\titlepagebackground[2][width=\paperwidth,height=\paperheight]{
	\def\@titlepagebackground{#2}
	\def\@titlepagebackgroundoptions{#1}
	}
	
\RequirePackage{eso-pic}

\renewcommand\titlepage{%
	\thispagestyle{empty}
	\AddToShipoutPictureBG*{%
		\put(0,0){\includegraphics[width=\paperwidth,height=\paperheight,page=1]
			{VSAreport/backgrounds/\@theme/titlepage.pdf}}%
		\ifx\@titlepagebackground\@empty\else
			\put(0,0){\expandafter\includegraphics\expandafter[\@titlepagebackgroundoptions]{\@titlepagebackground}}%
			\fi
		\put(0,0){\includegraphics[width=\paperwidth,height=\paperheight,page=2]
			{VSAreport/backgrounds/\@theme/titlepage.pdf}}%
		}	
	\newgeometry{right=1cm,left=1cm,top=.55\paperheight,bottom=1cm}
	\begin{flushright}
	\sffamily
	\null
	\vfill	
	{\fontsize{45}{40}\selectfont\bfseries\color{maincol1}\MakeUppercase{\@title}\par}%
	\ifx\@subtitle\@empty\else\vspace{5pt}{\fontsize{30}{10}\selectfont\color{maincol1}\@subtitle\par}\fi%
	\ifx\@author\@empty\else\vspace{10pt}{\Large\color{black}\@author\par}\fi%
	\ifx\@date\@empty\else\vspace{3pt}{\Large\color{black}\@date\par}\fi%
	\vfill
	\end{flushright}
	\includegraphics[width=4cm]{VSAreport/entitylogos/\@theme.png}
	\clearpage
	\restoregeometry
	}



%\def\lastpage{
%	\newpage
%	\ifodd\value{page}\hbox{}\newpage\fi
%	\thispagestyle{empty}%
%	\pagecolor{VUBblue}%
%	\strut\vfill 
%	\setlength{\fboxsep}{0pt}
%	\hspace*{-1.3in}%
%	\makebox[0pt][l]{\raisebox{-\height}[0pt][0pt]{%
%			\colorbox{white}{%
%				\begin{minipage}[c][1.3in][c]{\paperwidth}
%					\hspace{1.22in}%
%					\includegraphics[height=0.08\paperwidth]{VUBbook/VUB}
%				\end{minipage}%
%			}
%	}}
%	\clearpage
%	\nopagecolor  
%}
%













%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%  
%%%% SECTION TITLES   
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

\RequirePackage[explicit,newparttoc]{titlesec}	
\RequirePackage{needspace}

%\NewEnviron{parttitlepage}{
%	\thispagestyle{empty}%
%	\pagecolor{VUBblue}%
%	\strut\vfill 
%	\parbox[t][.6\paperheight]{10.5cm}{
%		\color{white}%
%		\BODY	
%	}
%	\makebox[0pt][l]{\raisebox{-\height}{%
%			\hspace{138pt}%
%			\includegraphics[height=.2\paperheight]{VUBbook/VUBtriangle}%
%	}}   
%}
%
%\titleformat{name=\part}{}{}{0pt}{%
%	\begin{parttitlepage}
%		\begin{flushleft}
%			\Large DEEL \thepart
%		\end{flushleft}
%		\vspace{1cm}
%		\begin{flushleft}
%			\titlefont\Huge #1
%		\end{flushleft}
%	\end{parttitlepage}%
%}[\clearpage\nopagecolor]
%
%\titleformat{name=\part,numberless}[display]{}{}{0pt}{%
%	\begin{parttitlepage}
%		\begin{flushleft}
%			\titlefont\Huge #1
%		\end{flushleft}
%	\end{parttitlepage}%                                                                                                
%}[\clearpage\nopagecolor]

\titleformat{name=\chapter}[display]
	{\sffamily\bfseries\color{maincol1}}
	{\large\chaptername\ \thechapter}
	{\baselineskip}
	{\huge\MakeUppercase{#1}}
	[\rule{\linewidth}{2pt}]
\titleformat{name=\chapter,numberless}[display]
	{\sffamily\bfseries\color{maincol1}}
	{}
	{0pt}
	{\addcontentsline{toc}{chapter}{#1}%
	 \huge\MakeUppercase{#1}%
	 }
	[\rule{\linewidth}{2pt}]	
\titlespacing*{\chapter}{0pt}{0pt}{5\baselineskip}	
	
	
\titleformat{name=\section}
	{%\needspace{5\baselineskip}
		\sffamily\Large\bfseries\color{maincol2}}
	{\thesection}
	{2ex}
	{\MakeUppercase{#1}}
\titlespacing*{\section}{0pt}{4\parskip}{\parskip}	

	
\titleformat{name=\subsection}
	{%\needspace{5\baselineskip}
		\sffamily\bfseries\color{maincol2}}
	{\thesubsection}
	{2ex}
	{#1}
\titlespacing*{\subsection}{0pt}{2\parskip}{\parskip}

\titleformat{name=\subsubsection}
	{%\needspace{2\baselineskip}
		\sffamily\itshape\color{maincol2}}
	{}
	{0pt}
	{#1}
\titlespacing*{\subsubsection}{0pt}{2\parskip}{\parskip}










%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%% TABLE OF CONTENTS
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

\RequirePackage{titletoc}   

\setcounter{tocdepth}{2}
 
\renewcommand\tableofcontents{%
	\thispagestyle{plain}
	\@starttoc{toc}
	\clearpage
	}
	
\titlecontents{part}
	[2.3em]
	{\addvspace{\baselineskip}\bfseries\color{maincol1}}
	{\contentslabel{2.3em}}
	{\hspace{-2.3em}}
	{\enspace\hrulefill \contentspage}
	[\addvspace{.5\baselineskip}]	
\titlecontents{chapter}
	[7mm]
	{\sffamily\vspace{\baselineskip}}
	{\contentslabel{7mm}}
	{}
	{ \titlerule*[2pt]{.}\contentspage} 
\titlecontents{section}
	[16mm]
	{\sffamily\small\color{textcol!80}}
	{\contentslabel{7mm}}
	{\hspace{7mm}}
	{ \hfill\contentspage} 
\titlecontents{subsection}
	[25mm]
	{\sffamily\small\color{textcol!60}}
	{\contentslabel{9mm}}
	{\hspace{9mm}}
	{ \hfill\contentspage} 
\titlecontents{subsection}
	[25mm]
	{\sffamily\small\color{textcol!60}}
	{\contentslabel{11mm}}
	{\hspace{11mm}}
	{ \hfill\contentspage} 




%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%% LIST OF TABLES AND FIGURES
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


\titlecontents{figure}[3.5em]{\smallskip}{\contentslabel{3.5em}\hspace*{-1em}}{}{\hspace{1ex}\textcolor{palgray4}{\hrulefill}\contentspage}
\titlecontents{table}[3.5em]{\smallskip}{\contentslabel{3.5em}\hspace*{-1em}}{}{\hspace{1ex}\textcolor{palgray4}{\hrulefill}\contentspage}

%% CORRECT LIJST VAN TABELLEN/FIGUREN 
\makeatletter
\renewcommand*{\ext@figure}{lot}%
\let\c@figure\c@table%
\let\ftype@figure\ftype@table%
\let\listoftablesandfigures\listoftables%
\AtBeginDocument{
	  \renewcommand{\listtablename}{Tabellen en figuren}
	  }
\makeatother










%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%% APPENDIX
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%  

\renewcommand\appendix{
	\clearpage
	\addcontentsline{toc}{section}{Appendices}
	\setcounter{subsection}{0}
	\renewcommand\thesubsection{\Alph{subsection}}
	\titleformat{name=\subsection}{}{}{0ex}{\bfseries\LARGE\MakeUppercase{\thesubsection\hspace{2ex}##1}}
	\titleformat{name=\subsection,numberless}{}{}{0ex}{\addcontentsline{toc}{subsection}{##1}\bfseries\LARGE\MakeUppercase{##1}}
	\titleformat{name=\subsubsection}{}{}{0ex}{\large\bfseries\MakeUppercase{\thesubsubsection\hspace{2ex}##1}}
	\titleformat{name=\subsubsection,numberless}{}{}{0ex}{\large\bfseries\MakeUppercase{##1}}
	}





%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%% FOOTNOTES
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%  

%\renewcommand{\@makefntext}[1]{%
%	\setlength{\parindent}{0pt}%
%	\begin{list}{}{%
%			\setlength{\labelwidth}{1.5em}%
%			\setlength{\leftmargin}{\labelwidth}%
%			\setlength{\labelsep}{3pt}%
%			\setlength{\itemsep}{0pt}%
%			\setlength{\parsep}{0pt}%
%			\setlength{\topsep}{0pt}%
%			%		\setlength{\rightmargin}{0.2\textwidth}%
%			\footnotesize%
%		}%
%		\item[\@makefnmark\hfil]#1%
%	\end{list}%
%}











%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%% LISTS    		
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

\RequirePackage{enumitem}

\setlist{labelindent=3ex,leftmargin=6ex,labelsep=*,align=left}

\setlist[itemize,1]{label=\textcolor{maincol2}{$\bullet$}}
\setlist[itemize,2]{label=\textcolor{maincol3}{$\bullet$}}
\setlist[itemize,3]{label=\textcolor{maincol3}{+}}

\setlist[enumerate,1]{label=\textcolor{maincol2}{\arabic*.},ref={\arabic*}}
\setlist[enumerate,2]{label=\textcolor{maincol3}{\alph*.}  ,ref={\alph*}}
\setlist[enumerate,3]{label=\textcolor{maincol3}{\roman*.} ,ref={\roman}}

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










%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%% FLOATS
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


\usepackage{floatrow}
\floatsetup{
	font={sf,footnotesize},
	captionskip=.2\baselineskip,
	justification=raggedright,
	capposition=top
	}

\usepackage{caption}
\captionsetup{
	font={sf,footnotesize},
	labelfont={bf},
	justification=justified,
	singlelinecheck=off,
	labelsep=colon
	} 
%\let\c@table\c@figure
%\let\ftype@table\ftype@figure     

    







%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%% BIBLIOGRAPHY
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

\RequirePackage[style=authoryear,backend=biber]{biblatex}
\defbibheading{bibliography}[\bibname]{\chapter*{#1}}
\addbibresource{references.bib} 





%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%% TABULARS   
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

\RequirePackage{tabularray}









%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%% HYPERREFS
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

\RequirePackage[hidelinks]{hyperref}












%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%% SPECIAL BLOCKS OF TEXT
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


\newenvironment{info}{%
	\vspace{\baselineskip}
	\begin{adjustwidth}{20mm}{10mm}%
	\begin{flushleft}
	\itshape\footnotesize%
	\makebox[0pt][r]{\raisebox{-9mm}[1.5ex][0pt]{\includegraphics[width=10mm]{VSAreport/icons/denken}}\hspace{5mm}}\ignorespaces%
	}{%
	\end{flushleft}
	\end{adjustwidth}%
	\vspace{\baselineskip}
	}




%\newtcolorbox{info}[1][]{%
%	enhanced,
%	breakable,
%%	colback=palgray2,
%%	colframe=palgray4,
%%	coltext=textcol,
%%	boxrule=0.9pt,
%%	boxsep=5pt,
%%	arc=1pt,
%	left=15mm,
%%	leftrule=0.8mm,
%	overlay unbroken={
%		\node[anchor=north west, xshift=4pt, yshift=-4pt] at (frame.north west){\includegraphics[width=8mm]{VSAreport/icons/denken}};
%		},
%	overlay first={
%		\node[anchor=north west, xshift=4pt, yshift=-4pt] at (frame.north west){\includegraphics[width=8mm]{VSAreport/icons/denken}};
%		},
%	fontupper=\itshape\footnotesize,
%	#1
%	}



