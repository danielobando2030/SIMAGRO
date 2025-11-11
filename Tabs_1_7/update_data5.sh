## Change Permission in folder
sudo rm -rf  /srv/shiny-server/Abs7
sudo mkdir /srv/shiny-server/Abs7


## Change Permission in folder
sudo chown -R shiny:shiny /srv/shiny-server/Abs7

### Update files
sudo rsync -a --recursive  /home/rstudio/Tableros/Abs7/ /srv/shiny-server/Abs7 --delete

## Change Permission in folder
sudo chown -R shiny:shiny /srv/shiny-server/Abs7

### refresh shiny-server
sudo systemctl stop shiny-server
sudo systemctl start shiny-server
