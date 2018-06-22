#!/usr/bin/perl
#
# A filter to line up tables neatly - mainly for use from Vim
# Toby Thurston -- 28 Sep 2017 
#
# 1. Read the data from stdin into a "table" object
# 2. Munge the table according to the supplied list of verbs+options
# 3. Print the table out again to stdout
#
# Usage: table delim verb [options] verb [options] ...
#
# Delimiter either specified or worked out from context if omitted
# Normally just '  ' but in tex: & and \cr; in latex & and \\; etc...
#
# Verbs: xp              transpose rows and cells
#        sort col-list   sort by value of columns in the order given, use UPPERCASE to reverse
#        add function    add sum, mean, var, sd etc to foot of column
#        arr col-list    rearrange/insert/delete cols and/or do calculations on column values.
#        dp dp-list      round numbers in each col to specified decimal places
#        sf sf-list      round numbers in each col to specified significant figures
#        reshape wide | long   reshape table (for R etc)
#        make tex | latex | plain | csv | tsv | md   output in tex etc form
#        label           label columns with letters
#        wrap n          wrap columns in long table n times (default=2)
#        unwrap n        unwrap cols in wide table (default=half number of cols)
#        zip n           zip n rows together
#        unzip n         unzip into n * the current number of rows & 1/n columns.
#
# For full documentaion read (or better still extract) the POD at the end

use 5.008;
use strict;
use warnings;
use utf8; # for £ signs
use open qw[ :std :utf8 ];

use List::Util qw(min max sum shuffle);
use Math::Prime::Util qw(factor);
use Math::Round qw(nearest);
use Math::SigFigs;
use POSIX qw(floor ceil);
use Statistics::Descriptive;    # used for add functions

# following Cowlishaw, TRL p.136, but excluding octal (leading 0 and no point) but including 0 itself
#                     sign?    mantissa-------------->  exponent?
my $Number_atom = qr{ [-+]? (?:\d+\.\d*|\.\d+|0|[1-9]\d*) (?:E[-+]?\d+)? }ixmso;
my $Number_pattern   = qr{\A $Number_atom \Z}ixmso;
my $Interval_pattern = qr{\A\( ( $Number_atom ) \, ( $Number_atom ) \)\Z}ixsmo;
my $Date_pattern     = qr{\A ([12]\d\d\d)\D?([01]\d)\D?([0123]\d) \Z}ixmso; # make groups capture
my $Hrule_pattern    = qr{\A -+ \Z}ixmso; # just a line of --------------

my %Action_for = (
    xp      => \&transpose,
    sort    => \&sort_rows,
    add     => \&add_totals,
    arr     => \&arrange_cols,
    make    => \&set_output_form,
    gen     => \&append_new_rows,
    dp      => \&round_cols,
    sf      => \&sigfig_cols,
    label   => \&add_col_labels,
    reshape => \&reshape_table,
    reflow  => \&reshape_table,
    flow    => \&reshape_table,
    wrap    => \&wrap_table,
    unwrap  => \&unwrap_table,
    zip     => \&zip_table,
    unzip   => \&unzip_table,
    shuffle => \&shuffle_rows,
    ditto   => \&copy_down, 
);

# deal with the command line
my @agenda = ();
while (@ARGV) {
    push @agenda, split q{ }, shift @ARGV;
}

my $delim = @agenda ? shift @agenda : 2;
my $separator;

if ($delim =~ /\A[1-9]\Z/xms) {
    $separator = q{ } x $delim;
    $delim = qr/\s{$delim,}/xms;
}
elsif ($delim =~ /\A[a-z]{2,}\Z/xms) {
    unshift @agenda, $delim;
    $separator = q{  };
    $delim = qr/\s{2,}/xms;
}
elsif ($delim eq '|') {
    $separator = '|';
    $delim = qr/\|/xms;
}
elsif ($delim eq ',') {
    $separator = ', ';
}
else {
    $separator = qq{$delim };
    $delim     = qr/$delim/xms;
}

my $indent = 0;
my $eol_marker = q{};

# Read the data from stdin unless it's connected to the terminal.
# For normal usage from VIM STDIN will *not* be connected to the terminal,
# but will have the input lines we need.  For testing, we can run "perl table.pl"
# from the command line: in this situation we *don't* want to wait for STDIN.
my @input_lines = -t STDIN ? () : <STDIN>;

if (@input_lines) {
    chomp(@input_lines);

    # find the clear margin of blank space to the left of the table, ignoring any completely blank lines
    $indent = min( map { /^(\s*)\S/; length($1) } grep { !/^\s*$/ } @input_lines ) || 0;

    # recognize TeX and LateX delims automatically
    if ($input_lines[0] =~ /\&.*(\\cr.*|\\\\.*)\Z/xms) {
        $eol_marker = $1;
        $delim = qr/\s*\&\s*/xms;
        $separator = ' & ';
    }
}

# split the input lines into cells in $Table->{data}
my @money_cols = ();
my $Table = { rows => 0, cols => 0 };
for (@input_lines) {
    s/^\s*//; # remove leading space
    s/\s*$//; # remove trailing space
    s/\t/  /g; # remove tabs
    if ( $eol_marker ne q{} ) {
        $_ =~ s/\s*\Q$eol_marker\E\s*//iox;
    }
    if ( /^$/ || /$Hrule_pattern/ || /^\\noalign/ || /^\\intertext/ || /^\#/ ) {
        push @{$Table->{specials}->[$Table->{rows}]}, $_;
        next;
    }
    my @cells = $delim eq ',' ? csv_split($_) : split $delim;
    my $i=0;
    for (@cells) {
        if (/^([£€]\s?)/) {
            $money_cols[$i] ||= $1;
            $_ =~ s/$1//;
        }
        elsif ( /^\s*\$/ ) {
            $money_cols[$i] ||= '$';
            $_ =~ s/\$//;
        }
        else{
            $money_cols[$i] ||= '0';
        }
        $i++;
    }
    push @{$Table->{data}}, \@cells ;
    $Table->{rows}++;
    $Table->{cols} = max($Table->{cols},scalar @cells);
}

# work through the list of verbs
while (@agenda) {
    my $verb   = lc shift @agenda;
    my $option = @agenda ? shift @agenda : undef;
    if ( defined $option && exists $Action_for{lc $option}) {
        unshift @agenda, $option;
        $option = undef;
    }
    if ( exists $Action_for{$verb} ) {
        $Action_for{$verb}->($option)
    }
    else {
        warn "> $verb is not defined.  Try one of:\n";
        warn join " ", ">", keys %Action_for, "\n";
    }
}

# work out the widths and alignments, and add back any required £ signs
my @widths = (0) x $Table->{cols};
my @aligns = (0) x $Table->{cols};
for (my $c=0; $c<$Table->{cols}; $c++ ) {
    for (my $r=0; $r<$Table->{rows}; $r++ ) {
        next unless defined $Table->{data}->[$r][$c];
        if ( decomma($Table->{data}->[$r][$c]) =~ $Number_pattern ) {
            $aligns[$c]++;
            if (defined $money_cols[$c] && $money_cols[$c] ne '0') {
                $Table->{data}->[$r][$c] = $money_cols[$c] . $Table->{data}->[$r][$c];
            }
        }
        else {
            $aligns[$c]--;
        }
        $widths[$c] = max($widths[$c], length $Table->{data}->[$r][$c]);
    }
}

my $table_width = sum(0,@widths) + length($separator) * ($Table->{cols}-1);

for (my $c=0; $c<$Table->{cols}; $c++ ) {
    $widths[$c] *= -1 if $aligns[$c] < 0;
}

# print the table to stdout
for (my $r=0; $r<$Table->{rows}; $r++ ) {
    if ( defined $Table->{specials}->[$r] ) {
        for my $special_line ( @{$Table->{specials}->[$r]} ) {
            print q{ } x $indent;
            if ($special_line =~ $Hrule_pattern) {
                if ($eol_marker eq '\\cr') {
                    print '\\noalign{\\vskip2pt\\hrule\\vskip4pt}' ; # auto convert ---- to tex rule
                }
                else {
                    print '-' x $table_width; # expand or shrink -------- lines
                }
            }
            else {
                print $special_line;
            }
            print "\n";
        }
    }
    my $out = q{ } x $indent;
    for (my $c=0; $c<$Table->{cols}; $c++ ) {
        if (defined $Table->{data}->[$r][$c]) {
            my $val = $Table->{data}->[$r][$c];
            my $wd = $separator eq q{,} ? length($val) :
                     $separator eq "\t" ? length($val) :
                     $widths[$c];
            if ( $separator eq q{,} && index($val,$separator)>0) {
                $val = q{"} . $val . q{"};
            }
            $out .= sprintf "%*s", $wd, $val;
        }
        $out .=  $separator;
    }
    $out =~ s/\Q$separator\E\Z/ $eol_marker/;
    if ($separator eq '<td>') { # hack for html form
        $out = "<tr><td>$out";
    }
    # remove any trailing white space (always irritating)
    $out =~ s/\s*$//;
    print $out, "\n";
}

exit 0;

sub csv_split {
    my $input = shift;
    my @out = ();
    my $in_quote = 0;
    my $field = '';
    for my $c ( split //, $input ) {
        if ($c eq ',' && !$in_quote) {
            $field =~ s/^\s*//;
            $field =~ s/\s*$//;
            $field =~ s/^"//;
            $field =~ s/"$//;
            push @out, $field;
            $field = '';
            next;
        }
        if ($c eq '"') {
            $in_quote = 1 - $in_quote;
        }
        $field .= $c;
    }
    $field =~ s/^\s*//;
    $field =~ s/\s*$//;
    $field =~ s/^"//;
    $field =~ s/"$//;
    push @out, $field;
    return @out;
}

sub set_output_form {
    my $form_name = shift;
    if    ($form_name eq "tex")   { $separator = ' & '; $eol_marker = '\\cr' }
    elsif ($form_name eq "latex") { $separator = ' & '; $eol_marker = '\\\\' }
    elsif ($form_name eq "md")    { $separator = ' | '; $eol_marker = q{}    }
    elsif ($form_name eq "csv")   { $separator = q{,} ; $eol_marker = q{}    }
    elsif ($form_name eq "tsv")   { $separator = "\t" ; $eol_marker = q{}    }
    elsif ($form_name eq "html")  { $separator = '<td>'; $eol_marker = q{}   }
    elsif ($form_name eq "debug") { $separator = ' ! '; $eol_marker = '<<'   }
    else                          { $separator = q{  }; $eol_marker = q{}    }
}

sub transpose {
    my @transposed_tab;
    for my $row (@{$Table->{data}}) {
        for my $i (0 .. $Table->{cols}-1) {
            push(@{$transposed_tab[$i]}, $row->[$i] );
        }
    }
    $Table->{data} = \@transposed_tab;
    @$Table{'rows','cols'} = @$Table{'cols','rows'};
}

# Sort by column.  Create an extra temp col with "arr" for fancy sorting.
sub sort_rows {
    my $col_list = shift;
    if ($col_list =~ m{[a-zA-Z]+}xmsio) {
        for my $c (reverse split //, $col_list) {
            sort_rows_by_column($c)
        }
    }
    else {
        sort_rows_by_column($col_list)
    }
}

sub sort_rows_by_column {
    my $col = shift;

    my $reverse = 0;
    if    (!$col)             { $col = 0 }
    elsif ($col =~ /^[a-z]$/) { $col = ord($col)-ord('a') }
    elsif ($col =~ /^[A-Z]$/) { $col = ord($col)-ord('A'); $reverse++ }
    elsif ($col =~ /^\d+$/)   { $col -= 1 } # 0 indexed
    elsif ($col =~ /^-\d+$/)  { $col = $Table->{cols}+1+$col }
    else                      { $col = 0 }

    # check bounds
    $col = $col >= $Table->{cols} ? $Table->{cols}-1
         : $col <  0            ? 0
         :                        $col;

    my @sorted;

    if ($reverse) {
        @sorted = map  { $_->[0] }
                  sort { $b->[1] <=> $a->[1] || $b->[2] cmp $a->[2] }
                  map  { [$_, as_number_reversed($_->[$col]), uc($_->[$col]||"")] } @{$Table->{data}};
    }
    else {
        @sorted = map  { $_->[0] }
                  sort { $a->[1] <=> $b->[1] || $a->[2] cmp $b->[2] }
                  map  { [$_, as_number($_->[$col]), uc($_->[$col]||"")] }  @{$Table->{data}};
    }                                   # or "" to allow for blank cells...

    $Table->{data} = \@sorted;
}

sub as_number {
    my ($s) = @_;
    return 1e9 unless defined $s;
    return $s if $s =~ $Number_pattern;
    if ($s =~ m{\A\d\d\D\d\d\D\d\d\d\d\Z}) {
        $s = etos($s);
        $s =~ s/-//g;
        return $s;
    }
    return -1e9;
}

sub as_number_reversed {
    my ($s) = @_;
    return -1e9 unless defined $s;
    return $s if $s =~ $Number_pattern;
    if ($s =~ m{\A\d\d\D\d\d\D\d\d\d\d\Z}) {
        $s = etos($s);
        $s =~ s/-//g;
        return $s;
    }
    return 1e9;
}

sub add_col_labels {
    # add a row of labels for each column
    my $label = 'a';
    my @labels = ();
    for (1..$Table->{cols}) {
        push @labels, $label++;
    }
    $Table->{rows} = unshift @{$Table->{data}}, [ @labels ];
}

sub add_totals {
    # Add values of function to bottom of cols
    my $expr = shift || "sum";
    if ($expr eq "var") {
        $expr = "variance"
    }
    elsif ($expr eq "sd") {
        $expr = "standard_deviation"
    }

    my @new_stats_row = ();
    my $stat = Statistics::Descriptive::Full->new();

    for (my $c = 0; $c < $Table->{cols}; $c++ ) {
        $stat->clear();
        for (my $r = 0; $r < $Table->{rows}; $r++ ) {
            my $s = $Table->{data}->[$r][$c];
            if (defined $s) {
                if ($s =~ $Number_pattern) {
                    $stat->add_data($s);
                }
                elsif ($s =~ /negative\(($Number_atom)\)/ ) {
                    $stat->add_data(-$1)
                }
            }
        }
        my $value = $expr;
        if ( $stat->count > 0 ) {
            if ( $expr =~ m{\A([a-z_]+)\((\d+)\)\Z}ixmso ) {
                $value = $stat->$1($2);
            }
            else {
                $value = $stat->$expr;
            }
        }

        push @new_stats_row, $value;
    }
    push @{$Table->{data}}, [ @new_stats_row ];
    $Table->{rows}++;
}

sub append_new_rows {
    my $sequence_or_number = shift || "Nothing";
    my $alpha = 1;
    my $omega = 10;
    if ( $sequence_or_number =~ m{\A-?\d+\Z}ixmso ) {
        $omega = $sequence_or_number;
    }
    elsif ( $sequence_or_number =~ m{\A(-?\d+)[^-0-9]+(-?\d+)\Z} ) {
        $alpha = $1;
        $omega = $2;
    }
    if ($alpha > $omega) {
        ($omega, $alpha) = ($alpha, $omega);
    }

    for my $n ($alpha .. $omega) {
        push @{$Table->{data}}, [ ($n) ];
        $Table->{rows}++;
    }
    $Table->{cols} = max(1, $Table->{cols});
}

sub shuffle_rows {
    $Table->{data} = [ shuffle @{$Table->{data}} ];
}

sub reshape_table {
    my $direction = shift;

    return if $Table->{cols}<3; # do nothing on thin tables

    if ( $direction eq "long" ) {
        make_long_table()
    }
    elsif ( $direction eq "wide" ) {
        make_wide_table()
    }
    else {
        if ($Table->{cols}==3) {
            make_wide_table()
        }
        else {
            make_long_table()
        }
    }
}

sub make_long_table {

    my @long_tab = ();
    my $header_row = shift @{$Table->{data}};

    push @long_tab, [ ($header_row->[0], 'Key', 'Value') ];

    for my $row ( @{$Table->{data}} ) {
        my $group_value = $row->[0];
        for my $i (1..(@$row-1) ) {
            push @long_tab, [ ( $group_value, $header_row->[$i], $row->[$i] ) ];
        }
    }

    $Table->{data} = \@long_tab;
    $Table->{rows} = scalar @long_tab;
    $Table->{cols} = 3;
}

sub make_wide_table {

    my $header_row = shift @{$Table->{data}};

    my %values = ();
    my @keys = ();
    my %seen = ();
    for my $row ( @{$Table->{data}} ) {
        my ($x, $y, $value) = @$row;
        $values{$x}{$y} = $value;
        push @keys, $y unless $seen{$y}++;
    }

    my @wide_tab = ();
    push @wide_tab, [ ($header_row->[0], @keys) ];
    for my $x ( sort keys %values ) {
        my @row = ( $x );
        for my $y ( @keys ) {
            push @row, $values{$x}{$y}
        }
        push @wide_tab, [ @row ];
    }

    $Table->{data} = \@wide_tab;
    $Table->{rows} = scalar @wide_tab;
    $Table->{cols} = 1 + scalar @keys;
}

sub sigfig_cols {
    my $sf_string = shift;

    # no-op unless we have a string of numbers
    return unless defined $sf_string && $sf_string =~ m{\A \d+ \Z}xims;

    # extend short string by repeating last digit
    my $lsf = length($sf_string);
    if ($lsf < $Table->{cols}) {
        $sf_string .= substr($sf_string, -1) x ($Table->{cols}-$lsf);
    }

    for my $row (@{$Table->{data}}) {
        my $i = 0;
        for my $cell (@{$row}) {
            my $sf = substr $sf_string, $i++, 1;
            next if !defined $cell;
            if ( $cell =~ $Number_pattern ) {
                $cell = FormatSigFigs($cell,$sf);
            }
            elsif ( $cell =~ $Interval_pattern ) {
                $cell = sprintf "(%s,%s)", FormatSigFigs($1,$sf), FormatSigFigs($2,$sf);
            }
        }
    }
}

sub round_cols {
    my $dp_string = shift;

    # no-op unless we have a string of numbers
    return unless defined $dp_string && $dp_string =~ m{\A \d+ \Z}xims;

    # extend short string by repeating last digit
    my $ldp = length($dp_string);
    if ($ldp < $Table->{cols}) {
        $dp_string .= substr($dp_string, -1) x ($Table->{cols}-$ldp);
    }

    for my $row (@{$Table->{data}}) {
        my $i = 0;
        for my $cell (@{$row}) {
            my $dp = substr $dp_string, $i++, 1;
            next if !defined $cell;
            if ( $cell =~ $Number_pattern ) {
                $cell = sprintf "%.${dp}f", $cell;
            }
            elsif ( $cell =~ $Interval_pattern ) {
                $cell = sprintf "(%.${dp}f,%.${dp}f)", $1, $2;
            }
        }
    }
}

sub comma {
    my $cell = shift;
    $cell = reverse $cell;
    $cell =~ s/(\d\d\d)(?=\d)(?!\d*\.)/$1,/g;
    return scalar reverse $cell; 
}

sub decomma {
    my $n = shift;
    $n =~ s/,//g;
    return $n;
}

sub copy_down {
    for (my $r = 0; $r < $Table->{rows}; $r++ ) {
        for (my $c=0; $c<$Table->{cols}; $c++ ) {
            if (defined $Table->{data}->[$r]->[$c] 
                && $r > 0 
                && $Table->{data}->[$r]->[$c] eq q{"}) {
                $Table->{data}->[$r]->[$c] = $Table->{data}->[$r-1]->[$c];
            }
        }
    }
}


sub arrange_cols {
    my $permutation = shift;

    if ( $permutation =~ m{\A~} ) {
        $permutation = substr('abcdefghijklmnopqrstuvwxyz',0,$Table->{cols})
                     . substr($permutation, 1);
    }
    elsif ( $permutation =~ m{\A-([a-z]+)\Z} ) {
        $permutation = substr('abcdefghijklmnopqrstuvwxyz',0,$Table->{cols});
        $permutation =~ s/$1//;
    }
    return unless $permutation;

    my %cumulative_sum_of = ();
    for (my $r = 0; $r < $Table->{rows}; $r++ ) {
        my $new_row_ref;
        my %value_for = ();  # build a hash of all the values
        my $key = 'a';       # indexed by column letter
        for (my $c=0; $c<$Table->{cols}; $c++ ) {
            my $value = $Table->{data}->[$r]->[$c] || 0;
            $cumulative_sum_of{$key} += $value if $value =~ m{$Number_pattern};
            if ($value =~ /$Number_pattern/ && $value<0 ) {
                $value = "($value)"
            }
            elsif ($value =~ /^([.1234567890]+)([BKMG])$/ ) {
                $value = sprintf "%g", $1 * ($2 eq 'G' ? 1073741824
                    : $2 eq 'M' ? 1048576
                    : $2 eq 'K' ? 1024
                    : 1);
            }
            elsif ($value !~ /$Number_pattern/)  {
                $value = "'$value'"
            }
            $value_for{$key} = $value;
            $key++; # bump the column index
        }
        for my $m ( $permutation =~ m{[a-zA-Z1-9.?\$]|\{.*?\}}gxmso ) {
            my $value;
            if ($m =~ /^[a-z]$/) { $value = $Table->{data}->[$r]->[ord($m)-ord('a')] }
         elsif ($m =~ /^[A-Z]$/) { $value = $cumulative_sum_of{lc $m} || $m }
         elsif ($m eq q{.})      { $value = $r+1 }
         elsif ($m eq q{$})      { $value = $Table->{rows} }
         elsif ($m eq q{?})      { $value = rand()-0.5 }
            else {
                    # strip {} from expr
                    $m =~ s/^\{//; $m =~ s/\}$//;
                    # substitute cell values (and don't bother checking for out of range letters)
                    my @tokens = $m =~ m/[a-z]+|[A-Z]+|./g;
                    for my $t (@tokens) {
                        if ( exists $value_for{$t} ) {
                            $t = $value_for{$t}
                        }
                        elsif ( exists $cumulative_sum_of{lc $t} ) {
                            $t = $cumulative_sum_of{lc $t}
                        }
                        elsif ( $t eq "_" ) {
                            $t = ".' '."
                        }
                    }
                    # evaluate & replace answer with expression on error
                    $value = eval join '', @tokens;
                    $value = $m if $@;
            }
            push @$new_row_ref, $value;
        }
        $Table->{data}->[$r] = $new_row_ref;
    }
    $Table->{cols} = scalar @{$Table->{data}->[0]};
}

sub wrap_table {
    my $n = shift || 2;
    return unless $n > 1;
    return unless $n < $Table->{rows}*$Table->{cols};

    my @wide_tab = ();
    my $new_cols = $Table->{cols} * $n;
    my $new_rows = ceil($Table->{rows}/$n);

    for (my $r=0; $r<$new_rows; $r++ ) {
        my @new_row = ();
        for (my $i=0; $i<$n; $i++ ) {
            for (my $c=0; $c<$Table->{cols}; $c++ ) {
                push @new_row, $Table->{data}->[$r+$i*$new_rows][$c];
            }
        }
        push @wide_tab, [ @new_row ];
    }

    $Table->{data} = \@wide_tab;
    $Table->{rows} = $new_rows;
    $Table->{cols} = $new_cols;

}

sub unwrap_table {
    my $n = shift || ceil($Table->{cols}/2);
    return unless $n > 0;
    return unless $n < $Table->{cols};

    my @thin_tab = ();
    my $new_cols = $n;
    my $new_rows = ceil($Table->{rows}*$Table->{cols}/$n);

    my $block = $n*$Table->{rows};

    for (my $r=0; $r<$new_rows; $r++ ) {
        my @new_row = ();
        for (my $c=0; $c<$new_cols; $c++ ) {
            my $cell_number = $r*$new_cols+$c;
            my $old_r = int ($cell_number % $block)/$new_cols;
            my $old_c = int ($cell_number / $block)*$new_cols + $cell_number % $new_cols;
            push @new_row, $Table->{data}->[$old_r][$old_c];
        }
        push @thin_tab, [ @new_row ];
    }

    $Table->{data} = \@thin_tab;
    $Table->{rows} = $new_rows;
    $Table->{cols} = $new_cols;

}

sub zip_table {
    my $n = shift || 2;
    my @new_table = ();
    my $new_cols = $Table->{cols} * $n;
    my $new_rows = ceil($Table->{rows} / $n);

    for (my $r=0; $r<$new_rows; $r++ ) {
        my @new_row = ();
        for (my $c=0; $c<$new_cols; $c++ ) {
            my $old_r = $n*$r + floor($c / $Table->{cols});
            my $old_c = $c % $Table->{cols};
            push @new_row, $Table->{data}->[$old_r][$old_c];
        }
        push @new_table, [ @new_row ];
    }

    $Table->{data} = \@new_table;
    $Table->{rows} = $new_rows;
    $Table->{cols} = $new_cols;
}

sub unzip_table {
    my $n = shift || 2;
    my @new_table = ();
    my $new_cols = ceil($Table->{cols} / $n);
    my $new_rows = $Table->{rows} * $n;

    for (my $r=0; $r<$new_rows; $r++ ) {
        my @new_row = ();
        for (my $c=0; $c<$new_cols; $c++ ) {
            my $old_r = floor($r / $n);
            my $old_c = $c + ($r % $n) * ceil($Table->{cols} / $n);
            push @new_row, $Table->{data}->[$old_r][$old_c];
        }
        push @new_table, [ @new_row ];
    }

    $Table->{data} = \@new_table;
    $Table->{rows} = $new_rows;
    $Table->{cols} = $new_cols;
}

# Useful functions
sub round {
    my ($n, $figs) = @_, 0;
    return nearest(10**(-$figs), $n)
}

# Day of the week from base
sub dow {
    my ($base) = @_;
    if ($base =~ $Date_pattern ) {
        $base = base($base);
    }
    if ($base =~ $Number_pattern) {
        # Note that in Perl % always gives an integer even if base is a float
        return qw(Mon Tue Wed Thu Fri Sat Sun)[$base%7]
    }
    return "DoW($base)";
}

# Convert dd/mm/yyyy to yyyy-mm-dd
sub etos {
    my ($edate) = @_;
    if ($edate =~ /([0123]\d)\D(0[1-9]|1[012])\D([12]\d\d\d)/) {
        return date(base("$3-$2-$1"))
    }
    else {
        return $edate;
    }
}
#
# Convert mm/dd/yyyy to yyyy-mm-dd
sub utos {
    my ($udate) = @_;
    if ($udate =~ /(0?[1-9]|1[012])\/(0?\d|[12]\d|3[01])\/([12]\d\d\d)/) {
        return date(base("$3-$1-$2"))
    }
    else {
        return $udate;
    }
}

# Convert yyyy-mm-dd to base date assuming Gregorian calendar rules
sub base {
    my $date = shift;
    my ($y,$m,$d);
    if (!defined $date || $date eq q{}) {
        (undef, undef, undef, $d, $m, $y) = localtime ;
        $y += 1900;
        $m -= 2;
    }
    elsif ($date =~ $Date_pattern ) {
        ($y, $m, $d) = ($1, $2, $3);
        $m -=3;
    }
    else {
        return "Base($date)";
    }
    while ($m<0)  { $y-=1; $m+=12 }
    while ($m>11) { $y+=1; $m-=12 }
    my $base=365*$y + floor($y/4) - floor($y/100) + floor($y/400) + floor(.4+.6*$m) + 30*$m + $d - 307;
    return $base;
}

# Gregorian-ymd:  returns "y-m-d" from a base number according
# to normal Gregorian calendar rules.  Like date('s',base,'b')
# but allows for negative base numbers, and returns y m d as a
# list.
#
sub date {
    my $d = shift || 0;
    my ($y, $m) = (0,0);
    $d = floor($d);

    # Assume anything less than 1000 is a delta on today. (including negative numbers).
    if ($d < 1000) {
        $d += base();
    }
    my $s = floor($d/146097); $d=$d-$s*146097;
    if ($d == 146096) { ($y, $m, $d) = ($s*400+400, 12, 31) } # special case 1
    else {
        my $c=floor($d/36524); $d=$d-$c*36524;
        my $o=floor($d/1461);  $d=$d-$o*1461;
        if ( $d==1460) { ($y, $m, $d) = ($s*400+$c*100+$o*4, 12, 31) } # special case 2
        else {
            $y=floor($d/365); $d=$d-$y*365+1; # d is now in range 1-365
            my @prior_days = ( $y==3 && ( $o < 24 || $c == 3 ) )
                ? (0, 31, 60, 91, 121, 152, 182, 213, 244, 274, 305, 335, 999)
                : (0, 31, 59, 90, 120, 151, 181, 212, 243, 273, 304, 334, 999);
            while ( $prior_days[$m] < $d) {
                $m++;
            }
            $d = $d - $prior_days[$m-1];
            $y = $s*400+$c*100+$o*4+$y+1;
        }
    }
    return sprintf "%d-%02d-%02d", $y, $m, $d;
}

# returns 01-12 from January-December
sub monthnumber {
    my $s = shift;
    my $m = index("JAN FEB MAR APR MAY JUN JUL AUG SEP OCT NOV DEC",uc(substr($s,0,3)));
    return sprintf "%02d", $m/4+1;
}

sub hms {
    my $s = shift;
    my $H = floor($s/3600); $s = $s - 3600 * $H;
    my $M = floor($s/60);   $s = $s - 60 * $M;
    my $S = floor($s);      $s = $s - $S;
    my $HMS = sprintf "%02d:%02d:%02d", $H, $M, $S;
    if ($s > 0) {
        $HMS .= sprintf ".%03d", $s * 1000
    }
    return $HMS
}


# parse a date into sortable form -- NB no _ in the function name
sub makedate {
    my $s = shift;
    if ( $s =~ m{\A (\d+) \s (\S+) \s ((?:19|20)\d\d) \Z}iosmx ) {
        my $m = monthnumber($2);
        my $d = sprintf "%02d", $1;
        my $date = "$3-$m-$d";
        my $base = base($date);
        return date($base);
    }
    elsif ( $s =~ m{\A([0123]\d)\D(0[1-9]|1[012])\D([12]\d\d\d) \Z}iosmx ) {
        return date(base("$3-$2-$1"));
    }
    elsif ( $s =~ m{\A([0123]\d)\D(0[1-9]|1[012])\D([012]\d) \Z}iosmx ) {
        return date(base("20$3-$2-$1"));
    }
    elsif ( $s > 1000000000000 ) {
        return date(base("1970-01-01") + floor($s/86400000)) . " " . hms(($s % 86400000)/1000)
    }
    elsif ( $s > 1000000000 ) {
        return date(base("1970-01-01") + floor($s/86400)) . " " . hms($s % 86400)
    }
    else {
        return $s;
    }
}

sub isoweek {
    my $s = shift;
    my $b = base($s);
    if ( $b =~ /\D/ ) {
        return $s
    }
    my $isoweek_year = substr date($b-3), 0, 4;
    my $start = first_monday($isoweek_year+1);
    if ($b < $start) {
        $start = first_monday($isoweek_year);
    }
    else {
        $isoweek_year++;
    }
    my $isoweek_number = 1 + int(($b-$start) / 7);
    my $isoday = 1 + $b % 7;
    return sprintf "%d-W%02d-%d", $isoweek_year, $isoweek_number, $isoday; 
}

sub first_monday {
    my $y = shift;
    my $dec28 = base(sprintf("%d1228", $y-1));
    my $dow = $dec28 % 7;
    return $dec28 + 7 - $dow;
}

# return months since Jan 2000 from mmm-yy
sub mondex {
    my ($s) = @_;
    my $m = monthnumber($s);
    my $y = substr($s,4,2);
    return 12*$y+($m-1)
}

# return "OK" or correct check digit for given ISBN 10
sub isbn10 {
    my $candidate = shift;
    my $candidate_check_digit = uc(substr($candidate,-1,1));
    my $check_sum = 0;
    for (my $i=0; $i<9; $i++) {
        $check_sum += (10-$i)*substr($candidate,$i,1);
    }
    my $check_digit = 11 - $check_sum%11;
    $check_digit = 0   if $check_digit == 11;
    $check_digit = 'X' if $check_digit == 10;
    if ($check_digit eq $candidate_check_digit) {
        return "OK $candidate_check_digit"
    }
    else {
        return $check_digit
    }
}

# return hh:mm from decimal hours
sub hhmm {
    my $h = shift;
    my $m = floor($h*60+0.5);
    return sprintf "%02d:%02d", $m/60, $m%60;
}

# return sec from h:m:s.ss
sub sss {
    my $ts = shift;
    my ($h, $m, $s) = split ':', $ts;
    return $s+60*$m+3600*$h;
}

__END__

=pod

=head1 NAME

Table --- a filter to line up tables nicely

This filter is primarily intended to be used as an assitant for the Vim editor.  The
idea is that you create a Vim command that calls table.pl on a marked area in your file
which is then replaced with the (improved) output of table.pl. It also works as a
simple command line filter.  The details of setting up Vim are explained below.

=head2 Motivation

If your work involves editing lots of plain text you will get familiar with a plain
text editor such as Vim or Emacs or similar. You will get familiar with the
facilities for arranging code and paragraphs of plain text.  Eventually you will
need to create a table of data or other information, something like this

      event  eruption  waiting
          1     3.600       79
          2     1.800       54
          3     3.333       74
          4     2.283       62
          5     4.533       85

and you may find that your editor has some useful facilities for working in "block"
mode that help to manage the table.  But you might also find that the facilities are
just a little bit limited.   You want to know the totals of each column, but you
really didn't want to load the data into a spreadsheet or a statistics system like
R; you just want the simple totals.   That's what table.pl is for.  Calling ":Table add"
creates this:

      event  eruption  waiting
          1     3.600       79
          2     1.800       54
          3     3.333       74
          4     2.283       62
          5     4.533       85
         15    15.549      354

OK, that's not perfect, but all you have to do now is change that 15 to "Sum" (or just
undo the last change to get rid of the new line or whatever).

Table.pl also lets you transpose a table (to get this...)

      event         1      2      3      4      5
      eruption  3.600  1.800  3.333  2.283  4.533
      waiting      79     54     74     62     85

as well as sort by any column in the table, rearrange the columns, delete columns,
or add new columns computed from the others.  It can't do everything you can do in a
spreadsheet but it can do most of the simple things, and you can use it right in the
middle of your favourite editor.

=head2 Design

The overall flow is as follows

1. Deal with the command line in a nice flexible way.

2. Read <STDIN> and parse each line into a row of table cells.

3. Update the table according to the verbs given on the command line.

4. Work out the widths and alignments for each column in the finished table.

5. Print the table neatly to <STDOUT>

Steps 4 and 5 tend to be the slowest.  Note that you don't have to supply any verbs;
so in this case step 3 takes no time at all, and the default action is therefore just
to line up your table neatly.  Text columns are aligned left, numeric columns aligned right.

=head1 USAGE

=head2 Use from the command line

You are unlikely to want to do this much, but try something like this

   cat somefile.txt | perl table.pl xp sort xp    # or whatever verbs you want

=head2 Setting up a Table command in Vim

Add a line like the following to your ".vimrc" file.

    :command! -nargs=* -range=% Table <line1>,<line2>!perl ~/perl-vim-table/table.pl <q-args>

which you should adjust appropriately so your perl can find where you put table.pl.
You can of course use some word other than "Table" as the command name. Take your pick,
except that Vim insists on the name starting with an uppercase letter.

With this definition, when you type ":Table" in normal mode in Vim, it will call table.pl
on the current area and replace it with the output.  If you are in Visual Line mode then
the current area will just be the marked lines.  If you are in Normal mode then the current
area will be the whole file.

From now on, I'm assuming you are using a Vim :Table command to access table.pl

=head2 Use from within VIM or GVim or MacVim, etc

    :Table [delimiter] [verb [option]]...

Use blank to separate the command line: the delimiter argument and any verbs or options must be
single blank-separated words.  Any word that looks like a verb will be treated as a verb, even
if you meant it to be an option.  See below for details.

The delimiter is used to split up each input line into cells.  It can be any string or regular
expression that's a valid argument to the perl C<split> function.  Except one containing blanks
or a whole number between 0 and 9.  You can't use blanks (even inside quotes) because of the
simple way that I split up the command line, and so I use whole numbers to mean "split on at least
that many consecutive blanks" so if you use 1 as an argument the line will be split on every
blank space, and so on. The default argument is 2.  This means the line will be split at every occurrence
of two or more blanks.  This is generally what you want.  Consider this example.

    Item          Amount
    First label       23
    Second thing      45
    Third one         55
    Total            123

In most circumstances you can just leave the delimiter out and let it default to two or more spaces.
Incidentally, any tab characters in your input are silently converted to double spaces before parsing.

There is one special case delimiter to assist working with CSV files.  If you specify a comma "," as the
delimiter, each line will be split up by my C<csv_split> function, that will treat commas inside "double quotes"
as part of a cell.  This is usually what you want.  If it's not then change the commas to some other character
and use that as the delimiter.

After the optional delimiter you should specify a sequence of verbs.  If the verb needs an option then
that goes right after the verb.  Verbs and options are separated by blanks.  The parsing is very simple.
If it looks like a verb it's treated as one.  If it doesn't, it's assumed to be an option.  Anything
coming after an option, but not recognized as a verb, causes an error.  A message will be written back in the file.
You will probably want to use the "undo" function after reading it.

=head1 DESCRIPTION

=head2 Verbs

In all the examples below you need to prefix the command with ":Table".  You can string
together as many verbs (plus optional arguments) as you like.

=over

=item xp - transpose the table

C<xp> just transposes the entire table. It takes no options.

    First   100
    Second  200
    Third   300

becomes

    First  Second  Third
      100     200    300

It's often useful in combination with verbs that operate on columns like C<sort> or C<add>.
So the sequence "C<xp add xp>" will give you row totals, for example.

=item add [sum|mean|sd|var|...] - insert the sum|mean|etc at the bottom of a column

C<add> adds the total to the foot of a column.  Or the mean, standard deviation, variance, etc.
The optional argument can be any valid method for a Statistics::Descriptive::Full object.  If you omit
the optional argument it defaults to "sum".   If you put "sd" it will be expanded to "standard_deviation",
similarly "var" is expanded to "variance".

Non-numerical entries in a column are simply ignored.

=item sort [a|b|c|...] - sort on column a|b|etc

C<sort> sorts the table on the given column.  "a" is the first, "b" the second, etc.
If you use upper case letters, "A", "B", etc the sort direction is reversed.
An index beyond the last column is automatically adjusted so "sort z" sorts on the last column
assuming you have fewer than 26 columns).

You can also use numbers, so "sort 2" sorts on the second column, while
like perl index addressing, "sort -1" means sort on the last column, "sort -2" last but one etc.
NB *unlike* perl index addressing, "sort 1" sorts the first column not the second.
(but "sort 0" also sorts on the first column...).  Because sorting is stable in perl, then
if you want to sort on column b then column a, you can do "sort a sort b" to get the desired
effect.

=item arr [arrange-expression] - rearrange the columns

At it simplest C<arr> lets you rearrange, duplicate, or delete columns.  So if you have a
four column table then:

=over

=item *

"C<arr dabc>" puts the fourth column first

=item *

"C<arr aabcd>" duplicates the first column

=item *

"C<arr cd>" deletes the first two columns

=item *

"C<arr abc>" keeps only the first three columns

=back

and so on.  If you want to keep everything and simply add an extra column at
the end, there's a shortcut to save you typing lots of column letters: "C<arr
~aa>" will keep B<all> the columns and then add two more copies of the first
one on the end.  If you want to do more complicated things with lots of
columns, you might find it easier to transpose the table first with "xp" and
then use the regular line editing facilities in Vim to rearrange the rows,
before transposing them back to columns.   You might also use the C<label> verb
to add alphabetic labels to the top of all the columns before you start.

Note: Astute readers may spot a problem here.  The sequence "arr add" meaning
"delete cols b and c and duplicate col d" won't work because "add" is a
valid verb.  In this case (as similar ones) just put a pair of empty braces
on the end, like so "arr add{}".

Besides letters to identify column values you can use "?" to insert a random number,
and "." to insert the current row number and "$" to insert the total number of rows.

You can also insert arbitrary calculated columns by putting an expression in curly braces.

=over

=item *

"C<arr ab{a+b}>" adds a new column that contains the sum of the values in the first two

=item *

"C<arr a{a**2}{sqrt(a)}>" adds two new cols with square and square root of the value in col 1.

=item *

"C<arr ~{sqrt(a)}>" keeps all existing cols and adds a new col with the square root of the value in col 1.

=back

and so on.  Each single letter "a", "b", etc is changed into the corresponding
cell value and then the resulting expression is evaluated. You can use any
normal Perl function: sin, cos, atan2, sqrt, log, exp, int, abs, and so on.
You can also use min, max (from List::Util) and floor and ceil from POSIX, as
well as a C<round()> function that works like Excel, and the C<nearest()> function
from Math::Round.

You can use operators like "." to concatenate values, but you can't include a
space in your formula because this confuses the command line processing
earlier.  So an extra operator is included: the operator "_" will concatenate
with a space.  Any sequence of more than one letter (like "bad") or any letter
that does not refer to a letter in your table (possibly like "z") will be
treated as a plain string.

Note that you should use lower case letters only to refer to each column value.
If you use an upper case letter, "A", "B", etc, it will be replaced by the
cumulative sum of the corresponding column, in other words the sum of the
values in the column from the top of the table to the current row. So given

    First   1
    Second  2
    Third   3

"C<arr abB>" gives you,

    First   1  1
    Second  2  3
    Third   3  6

Note that the upper case letters also work inside a {curly brace} expression, so you can include them in a
normal expression.

There are also some very simple date routines included.  C<base> returns the number of days
since 1 Jan in the year 1 (assuming the Gregorian calendar extended backwards).  The argument
should be blank for today, or in the form "yyyy-mm-dd".  C<date> does the opposite: given
a number that represents the number of days since the year dot, it returns the date in "yyyy-mm-dd" form.
There's also C<dow> which takes a date and returns the day of the week, as a three letter string.

So given a table with a column of dates, like this

    2011-01-17
    2011-02-23
    2011-03-19
    2011-07-05

the command "arr a{dow(a)}" creates this

    2011-01-17  Mon
    2011-02-23  Wed
    2011-03-19  Sat
    2011-07-05  Tue

alternatively "arr a{base()-base(a)}" will produce the days from each date to today.

    2011-01-17  1681
    2011-02-23  1644
    2011-03-19  1620
    2011-07-05  1512

and "arr a{date(base(a)+140)}" will add 20 weeks to each date

    2011-01-17  2011-06-06
    2011-02-23  2011-07-13
    2011-03-19  2011-08-06
    2011-07-05  2011-11-22

As a convenience is the number given to "date()" is less than 1000, then it's assumed that you mean
a delta on today rather than a day in the pre-Christian era.  So "date(70)" will produce the date in 10 weeks time,
and "date(-91)" will give you the date three months ago, and so on.  "date()" produces today's date.

Note: dates will also be recognized in the form yyyymmdd or yyyy/mm/dd, etc.  The exact matching expression is

    \A([12]\d\d\d)\D?([01]\d)\D?([0123]\d)\Z

so you must use 4 digit years, but you can use any non-digit as a separator.  It also means
that you can use dates like 2011-12-32.  You'll find that date(base('2011-12-32')) returns '2012-01-01'.

To get dates in this sorted format (which is the ISO standard by the way), you can use C<etos> and C<utos>.
C<etos> takes European dates in the form dd/mm/yyyy and returns yyyy-mm-dd; C<utos> takes US dates in the form mm/dd/yyyy.
Note that they still both expect full year numbers.  There's only so much automation that's worth while.

There's also a C<month_number> function that takes a string that looks like a month and returns an appropriate number.


=item dp [nnnnn...] - round numbers to n decimal places

As delivered table.pl calculates with 12 decimal places, so you might need to round your answers a bit.
This is what C<dp> does.  The required argument is a string of digits indicating how many decimal places
between 0 and 9 you want for each column.  There's no default, it just does nothing with no argument, but
if your string is too short the last digit is repeated as necessary.  So to round everything to a whole number
do "dp 0".  To round the first col to 0, the second to 3 and the rest to 4 do "dp 034", and so on.

=item make [plain|tex|latex|csv|tsv] - set the output format

C<make> sets the output format.   Normally this happens automagically, but if, for example, you want to separate
your input data by single spaces, you might find it helpful to do ":Table 1 make plain" to line everything up
with the default two spaces.   Or you might want explicitly to make a plain table into TeX format.

Note that this only affects the rows, it won't magically generate the TeX or LaTeX table preamble.

The CSV option should produce something that you can easily import into Excel
or similar spreadsheets.  However beware that it's not very clever: fields with
commas in will be enclosed with "double quotes", but my routines are designed
to be simple rather than fool proof.  To get back from CSV form to plain form
do C<Table , make plain>, (or just the undo command in Vi). 

The TSV option can be used when you want to import into Word -- you can use Table.. Convert Text to Table...
using tabs as the column separator

=item reshape [long|wide] - expand or condense data tables for R

This is used to take a square table and make it a long one.  It's best explained with an example.

Consider the following table.

    Exposure category     Lung cancer  No lung cancer
    Asbestos exposure               6              51
    No asbestos exposure           52             941

Nice and compact, but the values are in a 2x2 matrix rather than a useful column.  Sometimes you want
them to look like this.

    Exposure category     Key             Value
    Asbestos exposure     Lung cancer         6
    Asbestos exposure     No lung cancer     51
    No asbestos exposure  Lung cancer        52
    No asbestos exposure  No lung cancer    941

And that's what "reshape long" does.  Here's another example.

    Region      Quarter     Sales
    East        Q1          1200
    East        Q2          1100
    East        Q3          1500
    East        Q4          2200
    West        Q1          2200
    West        Q2          2500
    West        Q3          1990
    West        Q4          2600

With this input, "reshape wide" gives you this

    Region    Q1    Q2    Q3    Q4
    East    1200  1100  1500  2200
    West    2200  2500  1990  2600

Notice that parts of the headings may get lost in transposition.

=item wrap [n] | unwrap [n]

Another way to reshape a table.  Given

    East  Q1  1200
    East  Q2  1100
    East  Q3  1500
    East  Q4  2200
    West  Q1  2200
    West  Q2  2500
    West  Q3  1990
    West  Q4  2600

as input, "wrap" gives you

    East  Q1  1200  West  Q1  2200
    East  Q2  1100  West  Q2  2500
    East  Q3  1500  West  Q3  1990
    East  Q4  2200  West  Q4  2600

while "wrap 3" gives

    East  Q1  1200  East  Q4  2200  West  Q3  1990
    East  Q2  1100  West  Q1  2200  West  Q4  2600
    East  Q3  1500  West  Q2  2500

"unwrap" does the opposite - the option is the number of columns you want in the new output, and defaults
to half the number of columns in the input.

=item zip [n] | unzip [n]

Re-shape a table row by row.  Given

    Q1  East  1200
    Q1  West  2200
    Q2  East  1100
    Q2  West  2500
    Q3  East  1500
    Q3  West  1990
    Q4  East  2200
    Q4  West  2600

as input, "zip" gives you

    Q1  East  1200  Q1  West  2200
    Q2  East  1100  Q2  West  2500
    Q3  East  1500  Q3  West  1990
    Q4  East  2200  Q4  West  2600

"unzip" does the opposite.  The option is the number of rows to combine.  The default is 2, so that
you zip every other row, and unzip the table in half (as it were).

=item label - add alphabetic labels to all the columns

C<label> simply adds an alphabetic label at the top of the
columns to help you work out which is which when rearranging.

=item gen - generate rows

C<gen a..b> where C<a> and C<b> are integers, and C<..> is any non-numeric character sequence,
will generate a table with a single column of integers running from C<a> to C<b>.  C<gen 10> is
interpreted as C<gen 1..10>.

If the table already has some data, then the single column will be appended as new rows at the bottom
of the existing column "a".

=item shuffle - rearrange the rows with a Fisher-Yates shuffle.

This is implemented using the "shuffle" routine from List::Util.

Here's a one liner to generate a random 4x4 arrangement of the numbers 1 to 16:
(start with a blank file)

    :Table gen 16 shuffle wrap 4

produces (for example):

     5   7   4   9
    13   2   3   8
    15   1   6  16
    12  11  10  14

=back

=head2 Special rows

Any blank lines in your table are saved as special lines and reinserted at the
appropriate place on output. So if you have a long table you can use blanks
to separate blocks of data.  Similarly any lines consisting entirely of "-" characters
are treated as horizontal rules and reinserted (appropriately sized) on output.
Any lines starting with "#" are treated as comment lines, and again reinserted in the
right places on output.

=head2 Support for TeX and LaTeX

C<table.pl> also supports tables neatly in TeX and LaTeX documents.
To convert a plain table to TeX format use "make tex".  If you already have
a TeX table then C<table.pl> automatically spots the TeX delimiters "&" and "\cr",
and puts them back in when it formats the output. Everything else works as described above.
If you convert from plain to TeX format, then any horizontal rules will be converted
to the appropriate bit of TeX input to get a neat output rule.    No attempt is made to create
a preamble for you.

=head1 REQUIRED ARGUMENTS

None.  The default with no arguments is just to line up your table neatly.

=head1 OPTIONS

See the detailed description of the verbs and options in the L<DESCRIPTION> section above.

=head1 DIAGNOSTICS

Some errors will generate extra lines in the output, explaining what went wrong.
Bad errors may generate only lines of errors instead of your table data.
You can always get rid of them and back to a known postion by using the editor's C<undo> command.

=head1 EXIT STATUS

Not set.

=head1 CONFIGURATION

None.

=head1 DEPENDENCIES

Perl 5.08 or better.

Note that you don't actually need VIM compiled with Perl support, table.pl works entirely as an
external filter.

=head1 INCOMPATIBILITIES

Largely *because* it works as an external filter, table.pl is not very "Vim-like", so dyed-in-the-wool
Vim users may prefer other facilities for playing with columns of data.

=head1 BUGS AND LIMITATIONS

Probably plenty, because I've not done very rigorous testing.

=head1 AUTHOR

Toby Thurston -- 28 Sep 2017 

=head1 LICENSE AND COPYRIGHT

Same terms as Perl and VIM.  Free to use, but not to pass off as your own.
No warranty expressed or implied.

=cut
