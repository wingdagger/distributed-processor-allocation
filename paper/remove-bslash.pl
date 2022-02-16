#!/usr/local/bin/perl


$dir = "code";


&openFiles;
exit;


sub openFiles
{
    opendir (DIR, "$dir");
    @files = readdir (DIR);
    
    foreach $file (@files)
    {
	print "Reading $dir / $file \n";
	open (FILE, "$dir/$file");
	@lines = <FILE>;
	close (FILE);

	open (FILE, ">$dir/$file");

	foreach $line (@lines)
	{
	    if ($line =~ /\\/)
	    {
		print "$line";
		$line =~ s/\\/\\\\/g;
	    }
	    print FILE $line;
	}
	close (FILE);
    }





}
