package main

import (
	"flag"
	"fmt"
	"log"
	"os"
	"os/exec"
	"syscall"
)

func main() {
	ipCmd := flag.NewFlagSet("ip", flag.ExitOnError)
	ipBatchFile := ipCmd.String("f", "", "iproute2 batch file")
	nftCmd := flag.NewFlagSet("nft", flag.ExitOnError)
	nftFile := nftCmd.String("f", "", "nft file")

	if len(os.Args) < 2 {
		fmt.Println(`Expected "ip" or "nft" subcommand`)
		os.Exit(2)
	}

	switch os.Args[1] {
	case "ip":
		ipCmd.Parse(os.Args[2:])
		fmt.Printf("ip / %s\n", *ipBatchFile)
		cmd := exec.Command("/usr/bin/ip", "-b", *ipBatchFile)
		if *ipBatchFile == "-" {
			cmd.Stdin = os.Stdin
		}
		ipOutput, err := cmd.CombinedOutput()
		if err != nil {
			fmt.Println(string(ipOutput))
			log.Fatal(err)
		}
	case "nft":
		nftCmd.Parse(os.Args[2:])
		fmt.Printf("nft / %s\n", *nftFile)
		cmd := exec.Command("/usr/sbin/nft", "-f", *nftFile)
		if *nftFile == "-" {
			cmd.Stdin = os.Stdin
		}
		cmd.SysProcAttr = &syscall.SysProcAttr{}
		cmd.SysProcAttr.Credential = &syscall.Credential{Uid: uint32(0), Gid: uint32(0)}
		nftOutput, err := cmd.CombinedOutput()
		if err != nil {
			fmt.Println(string(nftOutput))
			log.Fatal(err)
		}
	default:
		fmt.Println(`Expected "ip" or "nft" subcommand`)
		os.Exit(2)
	}
}
