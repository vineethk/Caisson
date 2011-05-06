#!/bin/perl


$fp_bool = open(FPW, ">2.v"); #vlog
if( $fp_bool == 0 ) {
  print ("by PK open fail file is 2.v\n");#vlog
  exit(1);
} 
select(FPW);

$fp_bool = open(FPR, $ARGV[0]);
if( $fp_bool == 0 )
{
  print ("open fail $ARGV[0]\n");
}

$start="begin";#vlog
$stop ="end";  #vlog
$start_n=0;
$stop_n =0;

$start1="(";
$stop1 =")";
$start_n1=0;
$stop_n1 =0;

#vlog
$start2="task";
$stop2 ="endtask";
$start_n2=0;
$stop_n2 =0;

  
$start3="module";
$stop3 ="endmodule";
$start_n3=0;
$stop_n3 =0;

$start4="case";
$stop4 ="endcase";
$start_n4=0;
$stop_n4 =0;
#vlog


$offset=0;
$offset_step=2;
$offset_exp=0; #exceptional cases offset
$lineN=0; #current line number

read_line($line, $lineN);
while( $line ne "" ) { 

# select(stdout);
  #        0      1          2         3        4       5
  offset_U($line, $start_n,  $stop_n,  $start,  $stop,  $offset_exp);
  offset_U($line, $start_n1, $stop_n1, $start1, $stop1, $offset_exp);
  #vlog
  offset_U($line, $start_n2, $stop_n2, $start2, $stop2, $offset_exp);
  offset_U($line, $start_n3, $stop_n3, $start3, $stop3, $offset_exp);
  offset_U($line, $start_n4, $stop_n4, $start4, $stop4, $offset_exp);
  #vlog
# print("(start $start_n1 stop $stop_n1\n");
# print("{start $start_n  stop $stop_n \n");
# select(FPW);

  if($stop_n  > 0) { $offset -= ($offset_step * $stop_n);  }
  if($stop_n1 > 0) { $offset -= ($offset_step * $stop_n1); }
  if($stop_n2 > 0) { $offset -= ($offset_step * $stop_n2); }#vlog
  if($stop_n3 > 0) { $offset -= ($offset_step * $stop_n3); }#vlog
  if($stop_n4 > 0) { $offset -= ($offset_step * $stop_n4); }#vlog

  #vlog
  $line =~ s/case x /casex/g;
# $line =~ s/EEtask/endtask/g;
# $line =~ s/EEmodule/endmodule/g;
# $line =~ s/EEcase/endcase/g;
  #vlog
  trim_S($line, $offset, $offset_exp);

# select(stdout);
# print("$line");
  $temp = $line;
  $temp =~ s/^\t//g;
  $temp =~ s/ //g; 
# print("$temp");
# select(FPW);

  if($start_n  > 0) { $offset += ($offset_step * $start_n);  }
  if($start_n1 > 0) { $offset += ($offset_step * $start_n1); }
  if($start_n2 > 0) { $offset += ($offset_step * $start_n2); }#vlog
  if($start_n3 > 0) { $offset += ($offset_step * $start_n3); }#vlog
  if($start_n4 > 0) { $offset += ($offset_step * $start_n4); }#vlog
  #dbg
# if($lineN == 195 || $lineN == 196 || $lineN==197) {
#   prt_msg(
#     "start1 " . 
#     $start_n  . " " . $stop_n  . " " . 
#     $start_n1 . " " . $stop_n1 . " " . $offset, 
#     $lineN
#   );
# }
  #dbg
  


  read_line($line, $lineN);
}# end while 

sub trim_S {
  #make local
  my($i)=0;
  my($l)=length($_[0]);
  my($h)="";

  $line =~ s/^\t//g; #get rid of TABs.

  if( $_[0] =~ /'>/ ) {
    #specman specific. 
    #We should not insert any leading spaces or TABs.
    #We simply print it as it was in the file.
    print("$_[0]");
  }
  else {
    #Count spaces.
    while( substr($_[0], $i, 1) =~ / / && ($i < $l) ) {
      $i++;
    }
    $s=substr($_[0], $i, ($l-$i));
  
    #Handle negative offset values. Negative values are the result of special
    #remarks or code error i.e. unbalanced { or (.
    if($_[1] < 0 ) {
      prt_msg("negative value = " . $_[1] . "\^" . $_[0], $lineN);
      $_[1]=0;
    }

    #Add spaces.
    $h="";
    $i=$_[1] + $_[2];
#   print("...$i ");
    while($i > 0) {
      $i--;
      $h = $h . " ";
    }
  
    $result=$h . $s;
    print("$result");
  }
}
  
sub get_S {
  #make local
  my($i)=0;
  my($l)=length($_[0]);
  while( substr($_[0], $i, 1) =~ / / && $i < $l ) {
    $i++;
  }
}
  
#Works on line [0] and updates start and stop [1] and [2].
#0 line 1 start number 2 stop number 3 start char 4 stop char 
#5 offset exceptional
sub offset_U {
  $_[1]=0;
  $_[2]=0;
  $_[5]=0;
  #make local
  my($k)=0; # string under search pointer. 
  my($loff) =length($_[0]);
  my($k1)=length($_[3]);
  my($k2)=length($_[4]);
  my($my_line)="";

  #Clean line/line tail from VHDL/Verilog remark (// or --).
  rm_remarks($_[0], $my_line); 
  #exceptional event
# if($my_line =~ /}.*else.*{/) { 
  #vlog
  if($my_line =~ /\bbegin\b.*\bend\b/) {
    $_[1] = 0;
    $_[2] = 0;
    $_[5] = 0;
  }
  #vlog
  else { #regular
    while($k < $loff) {
     #if(substr($my_line, $k, $k1) =~ /$_[3]/) {}#Is it a begin 
      if(substr($my_line, $k, $k1) eq $_[3]) { #Is it a begin 
#old 
#       $_[1]++;
#new 
        match_ok($res, $my_line, $k, $_[3]);
        if($res == 0) {$_[1]++;}
        $k += $k1;
      }
      else {
       #if(substr($my_line, $k, $k2) =~ /$_[4]/) {}#Is it an end   
        if(substr($my_line, $k, $k2) eq $_[4]) { #Is it an end   
#old 
#         $_[2]++;
#new 
          match_ok($res, $my_line, $k, $_[4]);
          if($res == 0) {$_[2]++;}
          $k += $k2;
        }
        else { #This is nither begin nor end. 
          $k++;
        }
      }
    } #end while 
    #Prepare results and finish. 
    if($_[1] == $_[2]) {
      $_[1] = 0;
      $_[2] = 0;
    }
    else {
      if($_[1] > $_[2]) {
        $_[1] = $_[1] - $_[2];
        $_[2] = 0;
      }
      else {
        $_[2] = $_[2] - $_[1];
        $_[1] = 0;
      }
    }
  } #end of regular else
}


sub read_line {
  my($buf)="";
  $_[1]++;
  $_[0]=<FPR>;
  $_[0]=~ s/^\t//g;
  $_[0]=~ s/\t/ /g;
  #vlog
  $_[0]=~ s/case\(/case \(/g;
  $_[0]=~ s/begin:/begin :/g;
  $_[0]=~ s/:begin/: begin/g;
  $_[0]=~ s/casex/case x /g;
  if($_[0] !~ /^\s*\/\//) {#does not work on a remark
    if($_[0] =~ /\s*input \s*/ || $_[0] =~ /\s*output \s*/) {
      if($_[0] =~ /\s*input \s*(\[.*\])\s*([a-zA-Z0-9_]*)\s*(;.*)/) {
        $buf="  input  " . $1; while(length($buf) < 17) {$buf=$buf . " ";}
        $buf=$buf . $2 . "$3\n";
      }
      elsif($_[0] =~ /\s*input \s*([a-zA-Z0-9_]*)\s*(;.*)/) {
        $buf="  input          " . $1 . "$2\n";
      }
      elsif($_[0] =~ /\s*output \s*(\[.*\])\s*([a-zA-Z0-9_]*)\s*(;.*)/) {
        $buf="  output " . $1; while(length($buf) < 17) {$buf=$buf . " ";}
        $buf=$buf . $2 . "$3\n";
      }
      elsif($_[0] =~ /\s*output \s*([a-zA-Z0-9_]*)\s*(;.*)/) {
        $buf="  output         " . $1 . "$2\n";
      }
      else { $buf=$_[0]; }
      $_[0]=$buf;
    }
  }
# $_[0]=~ s/endtask/EEtask/g;
# $_[0]=~ s/endmodule/EEmodule/g;
# $_[0]=~ s/endcase/EEcase/g;
  #vlog
}


#Clean line/line tail from VHDL/Verilog remark (// or --).
sub rm_remarks {
  #make local
  my($rl)=length($_[0]);
  my($ri)=0;

  #position in string of verilog remark //, vhdl remark --, and string "
  my($str_pos)=0;

  $_[1]=$_[0]; #$_[1] is the result.
  #Fix string problem. When the // or -- are enclosed in a "..." string,
  #we should not treat it as a remark.
  if($_[0] =~ /\"/ ) {
    $str_pos=index($_[0], "\"", 0);
  }
  else {
    $str_pos=length($_[0]) + 1;
  }

  #remove remark
  if($_[0] =~ /\/\//) {
    if( index($_[0], "\/\/", 0) < $str_pos ) {
      @ra=split(/\/\//, $_[0]);
      $_[1]=$ra[0];
    }
  }
  else {
    if($_[0] =~ /--/) {
      if( index($_[0], "--", 0) < $str_pos ) {
        @ra=split(/--/, $_[0]);
        $_[1]=$ra[0];
      }
    }
  }

}

# 0 string to print to screen.
# 1 line number in file under test. 
sub prt_msg {
  select(stdout);
  print("$_[0] at line $_[1]\n");
  select(FPW);
}


#return result 0 ok $_[0] 
#line               $_[1] 
#location k         $_[2] 
#pattern            $_[3] 
#okay is returned if 
# ^pattern_ OR ^pattern$ OR _pattern$ OR _pattern_ where _ means space.
sub match_ok {
  $_[0] = 1; #defualt is FALSE result
  #make variable local 
  my($ltmp) = $_[1];
  my($l) = 0;
  my($search) = "i";
  my($dbg) = "i";

  chomp($ltmp);

  #The sub works only on patterns with length greater than 1.
  if(length($_[3]) == 1) {
    $_[0] = 0; 
  }
  else {
    $l = length($_[3]);
    if($_[2] == 0) {#match at line start
      $search = $_[3] . " ";
      if(substr($ltmp, $_[2], ($l+1)) eq $search) {
        $_[0] = 0;
      }
      if($l == length($ltmp)) {
        $_[0] = 0;
      }
    }
    else {
      $tail = (length($ltmp) - $l - 1);
      if( ($_[2]+$l) >= length($ltmp) ) {#match at line end 
        $search = " " . $_[3];
        if( substr($ltmp, ($_[2]-1), ($l+1)) eq $search ) {
          $_[0] = 0;
        }
      }
      else {
        #somewhere in the middle : ... end .....
        $search = " " . $_[3] . " ";
        if( substr($ltmp, ($_[2]-1), ($l+2)) eq $search ) {
          $_[0] = 0;
        }
      }
    }
  }
  #debug
  if($_[0] != 0) {
    $dbg = substr($ltmp, $_[2]);
#   print("debug \n$ltmp\n$dbg $_[2]\n pat=$_[3] \ndbg \n");
  }
}




