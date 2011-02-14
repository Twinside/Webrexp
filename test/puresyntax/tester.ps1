function autoTest(  )
{
    ls valid/*.webrexp | % {
        $file = $_
        ../../dist/build/Webrexp/webrexp --dot -f $file.fullname > temp

        if ( $LastExitCode -ne 0 )
        {
            echo "======================================"
            echo "Failed valid $($_.name)"
            echo "======================================"
            cat $file
        }
        else { echo "." }
    }

    ls invalid/*.webrexp | % {
        $file = $_
        ../../dist/build/Webrexp/webrexp --dot -f $_.fullname

        if ( $LastExitCode -eq 0 )
        {
            echo "======================================"
            echo "Valid invalid $($_.name)"
            echo "======================================"
            cat $file
        }
        else { echo "." }
    }

    rm temp
}

function testFile( $f )
{
    echo "======================================"
    echo "$f"
    echo "======================================"
    cat $f
    echo ""
    echo "======================================"
    ../../dist/build/Webrexp/webrexp --dot -f $f
}

if ( $args.length -gt 0 )
{
    testFile $args[0]
}
else { autoTest } 

