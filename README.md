# perl-vim-table

Toby Thurston -- 02 Jun 2017 

A perl filter that parses text and lines things up in columns.  

Intended to be used as a filter for vim.

Installation
------------

- Copy table.pl to a convenient folder, or try:

        git clone https://github.com/thruston/perl-vim-table.git

- Add these perl libraries (unless you already have them):

        cpanm Statistics::Descriptive
        cpanm Math::Prime::Util  
        cpanm Math::SigFigs
        cpanm Math::Round

- Add a line like the following to your ".vimrc" file.

        :command! -nargs=* -range=% Table <line1>,<line2>!perl ~/your-folder/table.pl <q-args>

  which you should adjust appropriately so your perl can find where you put table.pl.

  You can of course use some word other than "Table" as the command name. Take your pick, 
  except that Vim insists on the name starting with an uppercase letter.


For more details
----------------

        perldoc table.pl


