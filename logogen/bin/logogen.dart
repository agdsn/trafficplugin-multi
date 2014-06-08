library logogen;

import "dart:io";

import "package:image/image.dart";

void main() {
  final Image logo = decodeImage(new File("agdsn.png").readAsBytesSync());
  final Image inactive = brightness(logo.clone(), 200);
  
  final File inactive_file = new File("../logos/inactive.png");
  inactive_file.writeAsBytes(encodePng(inactive));
  
  final File active_file = new File("../logos/active.png");
  active_file.writeAsBytes(encodePng(logo));
  
  for(int percent = 0; percent <= 100; percent += 5){
    final Image logoprogress = logo.clone();
    
    const threshold_logo_red = 40;
    
    if(percent <= threshold_logo_red){
      colorOffset(logoprogress, ((threshold_logo_red - percent) * 
                                 (255 / threshold_logo_red)).round(), 0, 0, 0);
    }
    
    for(int x = ((128 - 5) * (percent / 100)).round(); x >= 5; x--){
      for(int y = 128 - 20 - 5; y <= 128 - 5; y++){
        logoprogress.setPixelRGBA(x, y, (255 - percent * 2.5).round(), (percent * 2.5).round(), 0);
      }
    }
    
    final File progress_file = new File("../logos/${percent}.png");
    progress_file.writeAsBytes(encodePng(logoprogress));
  }
}
