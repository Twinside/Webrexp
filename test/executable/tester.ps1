function autoTest(  )
{
    ls *.webrexp | % {
        $file = $_
        $rez = ../../dist/build/Webrexp/webrexp -q -f $file

        if ( $LastExitCode -ne 0 )
        {
            echo "======================================"
            echo "Failed valid $($_.name)"
            echo "======================================"
            cat $file
        }
        else
        {
            echo "."
        }

        $rezFile = $_.fullname + ".txtout"
        if ( test-path $rezFile )
        {
            $waited = cat $rezFile

            if ( compare-object $rez $waited )
            {
                echo "======================================"
                echo "Wrong results $($_.name)"
                echo "======================================"
                cat $file
                echo "=================== Waited:"
                echo $waited
                echo "=================== but got:"
                echo $rez
                echo "=================== diff :"
                compare-object $waited $rez
            }
        }
    }
}

function testFile( $f )
{
    echo "======================================"
    echo "$f"
    echo "======================================"
    cat $f
    echo ""
    echo "======================================"
    ../../dist/build/Webrexp/webrexp -v -f $f
}

if ( $args.length -gt 0 )
{
    testFile $args[0]
}
else { autoTest } 

