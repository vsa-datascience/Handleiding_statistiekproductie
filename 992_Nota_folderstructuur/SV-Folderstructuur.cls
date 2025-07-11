\NeedsTeXFormat{LaTeX2e}
\ProvidesClass{SV-Folderstructuur}

\LoadClass{SVnote}
\sloppy



%%% disable hyphenation for transformation pdf to docx. delete package for final document.
\usepackage[none]{hyphenat}




%%%% REMARKS
\usepackage{changepage}
\newenvironment{remark}{
    % \begin{adjustwidth}{1cm}{1cm} 
    \begin{flushleft}
    \small\itshape\color{pal2col2}
    }{
    \end{flushleft}
    % \end{adjustwidth}
    }




%%%% PACKAGES %%%%%%

\usepackage{tabularray}
\usepackage{pdflscape}
\usepackage{afterpage}


%%%% FOLDERSTRUCTURE %%%%%%%

\usepackage{fontawesome}
\usepackage{tikz}
\usepackage{etoolbox}
\newcounter{ypos}
\newenvironment{folderstructure}{
    \setcounter{ypos}{0}
    \newcommand\@iconnode[3]{
        \refstepcounter{ypos}
        \node[nodeicon,xshift=##1*3ex] (N##1) at (0,\theypos) {\textcolor{maincol2}{##3}};
        \node[nodetext,text=palgray4] at (N##1.east) {##2} ;
        \ifnumcomp{##1}{=}{0}{}{\draw[maincol3,dotted,very thick] (N\number\numexpr##1-1\relax.south) |- (N##1.west);}
        }
    \newcommand\folder[2]{\@iconnode{##1}{##2}{\faFolderOpen}}
    \newcommand\data[2]{\@iconnode{##1}{##2}{\faTable}}
    \newcommand\text[2]{\@iconnode{##1}{##2}{\faFileTextO}}
    \newcommand\code[2]{\@iconnode{##1}{##2}{\faFileCodeO}}
    \begin{tikzpicture}[
        y=-1.1\baselineskip,
        nodeicon/.style={anchor=west,inner sep=2pt,text height=1.2ex,text depth=0pt},
        nodetext/.style={anchor=west,font={\scriptsize\sffamily},inner sep=0pt,text height=1ex,text depth=0pt}
        ]
    }{
    \end{tikzpicture} 
    }



% \usepackage[edges]{forest}
% % \newcommand\dir[1]{\textcolor{maincol1}{\faFolderOpen}\makebox[0pt][l]{\ #1}}
% \newcommand\dir[1]{\textcolor{maincol1}{\faFolderOpen}\ #1}
% \forestset{
%   directory structure/.style={
%     for tree={
%       grow'=0,
%       child anchor=west,
%       parent anchor=west,
%       anchor=west,
%       align=left,
%       l sep=0pt, 
%       s sep=0pt, 
%       calign=first,
%       edge path={
%         \noexpand\path [draw,maincol2] (!u.south west) +(1.5ex,0) |- (.child anchor);
%         },
%       before typesetting nodes={
%         if n=1 {insert before={[,phantom]}}{}
%         },
%       fit=band,
%       before computing xy={l=15pt},
%       }  
%     }
%   }    