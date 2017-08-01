#! /bin/perl

use strict;
use warnings;
use Data::Dumper;

my $gridDir = "resource/grids";
my ($TRACE, $DEBUG, $INFO, $WARN, $ERROR) = (1,2,3,4,5);
my $buffer = "";

sub myprint {
  my $level = shift;
  return if $level < 3;
  $buffer .= ref $_ ? Dumper($_) : $_ foreach (@_);
}

sub printStat {
  my ($level, $stat) = @_;
  $stat->{Average} = 
    $stat->{Recursion} ? $stat->{Candidates} / $stat->{Recursion} : 0.0;
  myprint $level, "Statistics :\n", $stat, "\n";
}

sub printAllStat {
  my $all = shift;
  my $global = getNewStat();
  foreach (@$all) {
    $global->{Recursion} += $_->{Recursion};
    $global->{Candidates} += $_->{Candidates};
  }
  myprint $WARN, "Global ";
  printStat $ERROR, $global;
}

sub printGrid {
  my ($level, $grid) = @_;
  myprint($level, join(" ", @{$grid->[$_]}), "\n")
    foreach (0..8);
}

sub flush {
  print $buffer;
  $buffer = "";
}

sub getNewStat {
  my %stat = (
    Recursion => 0,
    Candidates => 0
  );
  return \%stat;
}

sub loadGrids {
  my $filesToLoad = shift;
  my @grids = ();

  foreach (@$filesToLoad) {
    myprint $TRACE, "Loading : $_\n";
    open my $in, '<', $_ or die "Could not read $_\n";
    my @lines = <$in>;
    my @grid = map {
      /(\d) (\d) (\d) (\d) (\d) (\d) (\d) (\d) (\d)/o ? 
        [$1,$2,$3,$4,$5,$6,$7,$8,$9] : ();
    } @lines;
    push @grids, \@grid;
    close $in;
  }
  return \@grids;
}

sub batchSolver {
  my @allStats = ();
  my $grids = shift;

  foreach (@$grids) {
    myprint $DEBUG, "Solving : \n";
    printGrid $DEBUG, $_;
    unless (isGridSolvable($_)) {
      myprint $WARN, "The initial grid is not solvable !\n";
      next;
    }

    push @allStats, getNewStat();
    my $solvedGrid = solveSingleGrid($_, $allStats[$#allStats]);

    unless ($solvedGrid) {
      myprint $WARN, "Could not solve the grid !\n";
      next;
    }
    myprint $INFO, "Solved !! Result :\n";
    printGrid $INFO, $solvedGrid;
    printStat $WARN, $allStats[$#allStats];
  }
  printAllStat \@allStats;
  flush();
}

sub isGridSolvable {
  my $grid = shift;
  for (my $i=0; $i<9; $i++) {
    my (%x, %y);
    for (my $j=0; $j<9; $j++) {
      return undef 
        if defined $x{$_->[$i][$j]} || defined $y{$_->[$j][$i]};   
      $x{$_->[$i][$j]} = 1 if $_->[$i][$j];
      $y{$_->[$j][$i]} = 1 if $_->[$j][$i];
    }
  }
  return 1;
}

sub solveSingleGrid {
  my ($grid, $stat) = @_;
  my ($move, $candidates);

  $stat->{Recursion}++;
  return $grid unless($move = getNextMove($grid));
  $candidates = getCandidates($grid, $move);

  foreach (@$candidates) {
    $stat->{Candidates}++;
    $grid->[$move->[0]][$move->[1]] = $_;
    return $grid if solveSingleGrid($grid, $stat);
    $stat->{Recursion}--;
    $grid->[$move->[0]][$move->[1]] = 0;
  }
  return undef;
}

sub getNextMove {
  my $grid = shift;
  my $move = undef;
  my $maxScore = 0;
  for (my $i=0; $i<9; $i++) {
    for (my $j=0; $j<9; $j++) {
      next if $grid->[$i][$j];
      my %score = map { ($grid->[$i][$_] => 1, $grid->[$_][$j] => 1) } (0..8);
      my $score = keys %score;
      if($score > $maxScore) {
        $move = [$i, $j];
        $maxScore = $score;
      }
    }
  }
  return $move;
}

sub notSoRandomList {
  my $move = shift;
  my @l = sort @{$_[0]};
  return \@l if @l < 2;
  unshift(@l, pop @l) foreach (1..$move->[0] % ($move->[1]+1));
  return \@l;
}

sub getCandidates {
  my ($grid, $move) = @_;
  my %candidates = map {
    ($grid->[$_][$move->[1]] => 1,
      $grid->[$move->[0]][$_] => 1)
  } (0..8); 
  my @res = grep (!$candidates{$_}, (1..9));
  return notSoRandomList $move, \@res;
}

############### MAIN #####################

my @roots = ('regular', 'easy', 'medium', 'hard');
my @files = map { glob "$gridDir/$_*" } @roots;

my $loadedGrids = loadGrids \@files;
batchSolver $loadedGrids;

