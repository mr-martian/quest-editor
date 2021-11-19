%top{
  #include <cstdio>
  #include <iostream>
  #include <iomanip>
%}

%class{
  int ch, wd, nl;
 public:
  void results() {
    out() << std::setw(8) << nl << std::setw(8) << wd << std::setw(8) << ch << '\n';
  }
%}


%init{
  ch = wd = nl = 0;
%}

%option unicode

line    \r?\n
word    (\w|\p{Punctuation})+

%%

{line}  ch += size(); ++nl;
{word}  ch += size(); ++wd;
.       ch += size();

%%

int main(int argc, char **argv) {
  std::FILE *fd = stdin;
  if (argc > 1 && (fd = std::fopen(argv[1], "r")) == nullptr)
    std::exit(EXIT_FAILURE);
  // create a lexer that consumes a file or reads stdin
  Lexer lexer(fd);
  // here we go!
  lexer.lex();
  // display the results
  lexer.results();
  if (fd != stdin)
    std::fclose(fd);
  return 0;
}