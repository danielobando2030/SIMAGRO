## Change Permission in folder
sudo rm -rf  /srv/shiny-server/Ind2
sudo mkdir /srv/shiny-server/Ind2


## Change Permission in folder
sudo chown -R shiny:shiny /srv/shiny-server/Ind2

### Update files
sudo rsync -a --recursive  /home/rstudio/Tableros/Ind2/ /srv/shiny-server/Ind2 --delete

## Change Permission in folder
sudo chown -R shiny:shiny /srv/shiny-server/Ind2

### refresh shiny-server
sudo systemctl stop shiny-server
sudo systemctl start shiny-server
