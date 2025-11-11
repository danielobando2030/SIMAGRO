## Change Permission in folder
sudo rm -rf  /srv/shiny-server/Ind3
sudo mkdir /srv/shiny-server/Ind3


## Change Permission in folder
sudo chown -R shiny:shiny /srv/shiny-server/Ind3

### Update files
sudo rsync -a --recursive  /home/rstudio/Tableros/Ind3/ /srv/shiny-server/Ind3 --delete

## Change Permission in folder
sudo chown -R shiny:shiny /srv/shiny-server/Ind3

### refresh shiny-server
sudo systemctl stop shiny-server
sudo systemctl start shiny-server
