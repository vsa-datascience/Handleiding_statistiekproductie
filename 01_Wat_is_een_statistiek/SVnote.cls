\NeedsTeXFormat{LaTeX2e}
\ProvidesClass{SVnote}

\LoadClass[12pt]{article}




%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%% GENERAL PACKAGES
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

\RequirePackage{graphicx}
\RequirePackage{etoolbox}
\RequirePackage[style={ddmmyyyy},datesep=.]{datetime2}
\RequirePackage{amssymb}
\RequirePackage{eso-pic}

\RequirePackage[dutch]{babel}





%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%% DEFINITIONS
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

\newcommand\printtitle{}
\newcommand\printshorttitle{}
\renewcommand\title[2][]{
	\renewcommand\printtitle{#2}
	\ifstrempty{#1}{
		\renewcommand\printshorttitle{#2}
		}{
		\renewcommand\printshorttitle{#1}
		}
	}
\newcommand\printsubtitle{}
\newcommand\subtitle[1]{\renewcommand\printsubtitle{#1}}
\newcommand\printauthor{}
\renewcommand\author[1]{\renewcommand\printauthor{#1}}	



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%% COLORS
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

\RequirePackage{xcolor}

\definecolor{maincol1}{HTML}{4B6C7A}
\definecolor{maincol2}{HTML}{71A3B8}
\definecolor{maincol3}{HTML}{8ED2EE}
\definecolor{textcol}{HTML}{34444A}

\definecolor{palgray1}{HTML}{EDEDED}
\definecolor{palgray2}{HTML}{E5E5E5}
\definecolor{palgray3}{HTML}{D5D5D5}
\definecolor{palgray4}{HTML}{989898}

\definecolor{pal1col1}{HTML}{4E7E71}
\definecolor{pal1col2}{HTML}{75C4AF}
\definecolor{pal1col3}{HTML}{79D9BF}

\definecolor{pal2col1}{HTML}{795032}
\definecolor{pal2col2}{HTML}{A66E45}
\definecolor{pal2col3}{HTML}{D18955}

\definecolor{pal3col1}{HTML}{767344}
\definecolor{pal3col2}{HTML}{A5A059}
\definecolor{pal3col3}{HTML}{C1BB67}

\color{textcol}




%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%% FONTS
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

\RequirePackage{fontspec}
\RequirePackage{anyfontsize}

\setmainfont{Calibri}

%\setmainfont{FlandersArtSerif-Regular.otf}[Path=SVnote/
%    ,BoldFont = FlandersArtSerif-Bold.otf
%    ,AutoFakeSlant=0.4
%    ]
%\setsansfont{FlandersArtSans-Regular.otf}[Path=SVnote/
%    ,BoldFont = FlandersArtSans-Bold.otf
%    ,ItalicFont = FlandersArtSans-Italic.otf
%    ,BoldItalicFont = FlandersArtSans-BoldItalic.otf 
%    ]
%\newfontfamily\medsans{FlandersArtSans-Medium.otf}[Path=SVnote/]
    
    
    
    
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%% PAGE LAY-OUT
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%   
    
\RequirePackage[]{geometry}
\geometry{
    ,a4paper
    ,twoside
    ,inner=35mm
    ,outer=35mm
    ,tmargin=35mm
    ,bmargin=35mm
    ,footskip=2\baselineskip
    }
\RequirePackage{parskip}
\widowpenalty=10000
\clubpenalty=10000
\sloppy
    
\setlength{\textfloatsep}{30pt plus 2.0pt minus 4.0pt}
\setlength{\floatsep}{30pt plus 2.0pt minus 2.0pt}   
\setlength{\intextsep}{30pt plus 2.0pt minus 2.0pt}  
    
    
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%% HEADERS AND FOOTERS
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%    
   
\RequirePackage{fancyhdr}   
\RequirePackage{lastpage}  
\pagestyle{fancy}
\fancyhf{} 
\renewcommand{\headrulewidth}{0pt}
\renewcommand{\footrulewidth}{1pt}
\patchcmd{\footrule}{\if@fancyplain}{\color{maincol2}\if@fancyplain}{}{}
\fancyfoot[L]{\footnotesize\printshorttitle}
\fancyfoot[R]{\footnotesize pagina \thepage\ van \pageref{LastPage}} 


 
   
    
    
    
    
    
    
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%% TITLE PAGE
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%  
    
\renewcommand\titlepage{
	\clearpage
	\newgeometry{lmargin=23mm,rmargin=15mm,vmargin=15mm}
	\thispagestyle{empty}
	\AddToShipoutPicture*{%
		\includegraphics[page=1,width=\paperwidth]{SVnote/backgroundtitlepage}%
		}
	\begin{flushleft}
	\includegraphics[width=50mm]{SVnote/Entiteitslogo_Statistiek_Vlaanderen}
	\vfill
	\bfseries
	\fontsize{50}{50}\selectfont\MakeUppercase{\printtitle}\par	
	\vspace{5mm}
	\LARGE
	\ifdefempty\printsubtitle{}{\printsubtitle\par}
	\large
	\ifdefempty\printauthor{}{\printauthor\par}	
	\DTMtoday
	\vfill
	\hfill\includegraphics[width=50mm]{SVnote/Vlaanderenissterkincijfers}
	\end{flushleft}
	\restoregeometry
    \newpage
	}
   
   
   

   
   
   
   
   

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%% TABLE OF CONTENTS
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%    
    
\RequirePackage{titletoc}    
\renewcommand\tableofcontents{%
    \@starttoc{toc}
    \vspace{4\baselineskip}
    }
\titlecontents{section}[7mm]{\vspace{\baselineskip}}{\contentslabel{7mm}}{}{ \titlerule*[2pt]{.}\contentspage} 
\titlecontents{subsection}[16mm]{\small\color{textcol!80}}{\contentslabel{7mm}}{\hspace{7mm}}{ \hfill\contentspage} 
\titlecontents{subsubsection}[25mm]{\small\color{textcol!60}}{\contentslabel{9mm}}{\hspace{9mm}}{ \hfill\contentspage} 

      
    
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%% SECTION TITLES
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%  

\RequirePackage[explicit]{titlesec}

\titleformat{name=\section}{\color{maincol2}}{}{0ex}{\bfseries\LARGE\MakeUppercase{\thesection\hspace{2ex}#1}}
\titleformat{name=\section,numberless}{\color{maincol2}}{}{0ex}{\addcontentsline{toc}{section}{#1}\bfseries\LARGE\MakeUppercase{#1}}
\newcommand{\sectionbreak}{\vspace{2\baselineskip}}

\titleformat{name=\subsection}{\color{maincol2}}{}{0ex}{\large\bfseries\MakeUppercase{\thesubsection\hspace{2ex}#1}}
\newcommand{\subsectionbreak}{\vspace{\baselineskip}}

\titleformat{name=\subsubsection}{\color{gray4}}{}{0ex}{\large\thesubsubsection\hspace{2ex}#1}




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

\renewcommand{\@makefntext}[1]{%
	\setlength{\parindent}{0pt}%
	\begin{list}{}{%
		\setlength{\labelwidth}{1.5em}%
		\setlength{\leftmargin}{\labelwidth}%
		\setlength{\labelsep}{3pt}%
		\setlength{\itemsep}{0pt}%
		\setlength{\parsep}{0pt}%
		\setlength{\topsep}{0pt}%
%		\setlength{\rightmargin}{0.2\textwidth}%
		\footnotesize%
		}%
	\item[\@makefnmark\hfil]#1%
	\end{list}%
	}


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



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%% FLOATS
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


\usepackage{floatrow}
\floatsetup{font={sf,footnotesize},captionskip=.2\baselineskip,justification=raggedright,capposition=top}

\usepackage{caption}
\captionsetup{font={sf,footnotesize,it}
             ,labelfont={bf}
             ,justification=justified
             ,singlelinecheck=off
             ,labelsep=colon
             } 
\let\c@table\c@figure
\let\ftype@table\ftype@figure     
    
    
    




%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%% TABULARS   
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

\usepackage{tabularx}
\newcolumntype{L}{>{\raggedright\arraybackslash}X}
\newcolumntype{R}{>{\raggedleft\arraybackslash}X}
\newcolumntype{C}{>{\centering\arraybackslash}X}

%\usepackage{tabularray}







%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%% BIBLIOGRAPHY
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

\RequirePackage[style=authoryear,natbib=true,backend=bibtex]{biblatex}
\defbibheading{bibliography}[\bibname]{\subsection{#1}}
    
    



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%% HYPERREFS
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

\RequirePackage[hidelinks]{hyperref}



