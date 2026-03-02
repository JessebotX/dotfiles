function toggle-theme {
    # .DESCRIPTION
    # Toggle dark theme
    # .NOTES
    # Version: 2022-01-15 2022-06-08
    # .LINK
    # http://xahlee.info/powershell/powershell_set_darktheme.html

    $xpath = "HKCU:\SOFTWARE\Microsoft\Windows\CurrentVersion\Themes\Personalize";
    $xname = "AppsUseLightTheme";

    if ((Get-ItemProperty $xpath -Name AppsUseLightTheme).$xname -eq 1) {
        Set-ItemProperty $xpath -Name $xname -Value 0;
    } else {
        Set-ItemProperty $xpath -Name $xname -Value 1;
    }
}

function y {
    $tmp = (New-TemporaryFile).FullName
    yazi.exe $args --cwd-file="$tmp"
    $cwd = Get-Content -Path $tmp -Encoding UTF8
    if (-not [String]::IsNullOrEmpty($cwd) -and $cwd -ne $PWD.Path) {
        Set-Location -LiteralPath (Resolve-Path -LiteralPath $cwd).Path
    }
    Remove-Item -Path $tmp
}

# Invokes a Cmd.exe shell script and updates the environment.

function Invoke-CmdScript {
    param(
        [String] $scriptName
    )
    $cmdLine = """$scriptName"" $args & set"
        & $Env:SystemRoot\system32\cmd.exe /c $cmdLine |
        select-string '^([^=]*)=(.*)$' | foreach-object {
            $varName = $_.Matches[0].Groups[1].Value
            $varValue = $_.Matches[0].Groups[2].Value
            set-item Env:$varName $varValue
    }
}

function Load-VisualCEnvironment {
    $vcvarsbatpath='C:\Program Files\Microsoft Visual Studio\18\Community\VC\Auxiliary\Build\vcvars64.bat'
    Invoke-CmdScript $vcvarsbatpath
}

function Get-ChildItemAll {
    ls -Force $args
}

## Aliases ##

Set-Alias -Name vc -Value Load-VisualCEnvironment
Set-Alias -Name ll -Value Get-ChildItemAll
Set-Alias -Name e  -Value nvim
Set-Alias -Name ff -Value y
Set-Alias -Name fj -Value yazi
Set-Alias -Name touch -Value New-Item

function prompt {
    $loc = $executionContext.SessionState.Path.CurrentLocation;

    $out = ""
    if ($loc.Provider.Name -eq "FileSystem") {
        $out += "$([char]27)]9;9;`"$($loc.ProviderPath)`"$([char]27)\"
    }
    $out += "PS $loc$('>' * ($nestedPromptLevel + 1)) ";
    return $out
}
